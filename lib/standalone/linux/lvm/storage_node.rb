require 'rubygems'
require 'systemu'
require 'activesupport'
require 'log4r'

class StorageNode
  def self.free_space
    `pvs --units g|grep mnekovg`.split("\n").first.split(" ")[5].to_f
  end
  
  def self.used_space
    `pvs --units g|grep mnekovg`.split("\n").first.split(" ")[4].to_f - free_space
  end
  
  def self.list_volumes()
    _, res = systemu 'lvs | grep mnekovg'
    lines = res.split("\n")
    lines.map {|line| line.split(' ').first}
  end
  
  def self.list_disks()
    volumes = self.list_volumes()
    res = []
    volumes.each do |v|
      if v =~ /vd\d/
        res << v.gsub('vd', '').to_i
      end
    end
    
    res
  end

  def self.volume_size(disk_number)
    _, res = systemu("lvs mnekovg/vd%d" % [disk_number])
    lines  = res.split("\n")
    return 0 if lines.size() < 2

    volume_record = lines[1]
    volume_attrs = volume_record.split(" ")
    volume_attrs[3].to_i
  end
  
  def self.volume_exists?(disk_number)
    all = list_volumes()
    name = "vd" + disk_number.to_s
    all.include?(name)
  end

  def self.create_volume(disk_number, size)
    cmd = "lvcreate -L#{size}GiB -n vd#{disk_number} mnekovg"
    Log4r::Logger['os'].debug("command: " + cmd)
    _, res, err = systemu(cmd)
    Log4r::Logger['os'].debug("output: " + res)
    Log4r::Logger['os'].debug("stderr: " + err)
    Log4r::Logger['os'].debug("done")

    err.blank?
  end

  def self.delete_volume(disk_number)
    cmd = "lvremove mnekovg/vd#{disk_number} --force"
    Log4r::Logger['os'].debug("command: " + cmd)
    _, res, err = systemu(cmd)
    Log4r::Logger['os'].debug("output: " + res)
    Log4r::Logger['os'].debug("stderr: " + err)
    Log4r::Logger['os'].debug("done")

    err.blank?
  end
  
  def self.list_backups(disk_number)
    result = []
    volumes = list_volumes()
    volumes.each do |volume|
      result << "%s-%s" % [$1, $2] if volume =~ /vd#{disk_number}-(\d{8})-(\d{2})/
    end
    
    result
  end
  
  def self.backup_details(disk_number, name)
    _, res = systemu "lvs|grep vd#{disk_number}-#{name}"
    data = res.split("\n").first.split(" ")
    {
      :volume => data[0],
      :size => data[3].to_i,
      :allocated => data[5].to_f
    }
  end
  
  def self.create_backup(disk_number, size = 1)
    date = Date.today.strftime("%Y%m%d")
    backup_number = (list_backups(disk_number).map {|b|
      b =~ /(#{date})-(\d{2})/
      $2.to_i
    }.max || 0) + 1

    volume_name = "vd%s-%s-%.2d" % [disk_number, date, backup_number]
    _, res, err = systemu "lvcreate -s -L#{size}g -n #{volume_name} mnekovg/vd#{disk_number}"
    err.blank?
  end
  
  def self.grow_backup(disk_number, name, size)
    grow_volume("vd%s-%s" % [disk_number, name], size)
  end
  
  def self.is_exported?(disk_number)
    _, res = systemu "ggaoectl stats vd#{disk_number}"
    !res.blank?
  end

  def self.add_export(disk_number, slot)
    name = "vd" + disk_number.to_s
    
    if !is_exported?(disk_number)
      _, res = systemu 'cat /etc/ggaoed.conf'
      lines = res.split("\n")
      result = lines
      
      result << "[#{name}]"
      result << "path = /dev/mnekovg/#{name}"
      result << "shelf = " + disk_number.to_s
      result << "slot = " + slot.to_s
      result << ""

      File.open("/etc/ggaoed.conf", "w") do |f|
        f.write(result.join("\n"))
      end

      restart_ggaoed()
      return true
    end
    
    false
  end

  def self.remove_export(disk_number)
    name = "vd" + disk_number.to_s
    
    if is_exported?(disk_number)
      _, res = systemu 'cat /etc/ggaoed.conf'
      lines = res.split("\n")
      result = []
      found = false
      lines.each do |line|
        found = false if line.strip =~ /\[\w+\]/ && found
        found = true if line.strip == "[#{name}]"
        result << line if !found
      end

      File.open("/etc/ggaoed.conf", "w") do |f|
        f.write(result.join("\n"))
      end

      restart_ggaoed()
      return true
    end
    
    false
  end

  private

  def self.grow_volume(volume, size)
    _, res, err = systemu "lvresize -L#{size}g mnekovg/#{volume}"
    err.blank?
  end

  def self.restart_ggaoed()
    systemu 'ggaoectl reload'
  end
end
