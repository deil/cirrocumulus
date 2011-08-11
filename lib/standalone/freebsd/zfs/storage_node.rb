require 'rubygems'
require 'systemu'
require 'activesupport'
require 'log4r'

class StorageNode

  VOL_NAME = "tank/vps"

  def self.free_space
    free_space_str = `zfs list #{VOL_NAME}`.split("\n").last.split(" ")[2]
    free_space = free_space_str.gsub(',', '.').to_f
    if free_space_str =~ /T/
      free_space = free_space * 1024
    end
    
    free_space
  end
  
  def self.used_space
    used_space_str = `zfs list #{VOL_NAME}`.split("\n").last.split(" ")[1]
    used_space = used_space_str.gsub(',', '.').to_f
    if used_space_str =~ /T/
      used_space = used_space * 1024
    end
    
    used_space
  end

  def self.list_volumes()
    cmd = "zfs list | grep #{VOL_NAME}/xen"
    Log4r::Logger['os'].debug("command: " + cmd)
    _, res, err = systemu(cmd)
    Log4r::Logger['os'].debug("output: " + res)
    Log4r::Logger['os'].debug("stderr: " + err)
    Log4r::Logger['os'].debug("done")

    lines = res.split("\n")
    lines.map {|line| line.split(' ').first.gsub("#{VOL_NAME}/", '') }
  end

  def self.list_disks()
    volumes = self.list_volumes()
    res = []
    volumes.each do |v|
      if v =~ /xen\-\d/
        res << v.gsub('xen-', '').to_i
      end
    end

    res
  end

  def self.volume_size(disk_number)
    0
  end

  def self.volume_exists?(disk_number)
    all = list_volumes()
    all.include?(zfs_disk_name(disk_number))
  end

  def self.create_volume(disk_number, size)
    disk_name = "%03d" % disk_number
    cmd = "zfs create -V #{size}G #{VOL_NAME}/xen-#{disk_name}"
    Log4r::Logger['os'].debug("command: " + cmd)
    _, res, err = systemu(cmd)
    Log4r::Logger['os'].debug("output: " + res)
    Log4r::Logger['os'].debug("stderr: " + err)
    Log4r::Logger['os'].debug("done")

    err.blank?
  end

  def self.delete_volume(disk_number)
    disk_name = "%03d" % disk_number
    cmd = "zfs destroy #{VOL_NAME}/xen-#{disk_name}"
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
    disk_name = "%03d" % disk_number
    res = `ps aux | grep xen-#{disk_name}`
    #Log4r::Logger['os'].debug("command: " + cmd)
    #_, res, err = systemu(cmd)
    Log4r::Logger['os'].debug("output: " + res)
    #Log4r::Logger['os'].debug("stderr: " + err)
    Log4r::Logger['os'].debug("done")
    
    res.split("\n").any? {|l| l =~ /vblade/ }
  end

  def self.add_export(disk_number, slot)
    disk_name = "%03d" % disk_number
    cmd = "/usr/local/sbin/vblade #{disk_number} #{slot} em1 /dev/zvol/#{VOL_NAME}/xen-#{disk_name}"
    
    if !is_exported?(disk_number)
      cmd = "/usr/local/sbin/vblade #{disk_number} #{slot} em1 /dev/zvol/#{VOL_NAME}/xen-#{disk_name} &"
      Log4r::Logger['os'].debug("command: " + cmd)
      _, res, err = systemu(cmd)
      Log4r::Logger['os'].debug("output: " + res)
      Log4r::Logger['os'].debug("stderr: " + err)
      Log4r::Logger['os'].debug("done")
      
      return err.blank?
    end
    
    false
  end

  def self.remove_export(disk_number)
    disk_name = "%03d" % disk_number
    out = `ps aux | grep xen-#{disk_name}`
    puts out
    out.split("\n").each do |l|
      if l =~ /vblade/
        pid = l.split(" ").second
        puts "pid: #{pid}"
        `kill #{pid}`
        
        return true
      end
    end
    
    false
  end

  private
  
  def self.zfs_disk_name(disk_number)
    "xen-%03d" % disk_number
  end

  def self.grow_volume(volume, size)
    _, res, err = systemu "lvresize -L#{size}g mnekovg/#{volume}"
    err.blank?
  end
end
