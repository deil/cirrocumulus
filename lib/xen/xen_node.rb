require 'rubygems'
require 'systemu'
require 'erb'

class XenNode
  def self.list_running_domUs()
    domus = []

    _, res = systemu 'virsh list'
    list = res.split("\n")
    list.each_with_index do |item,idx|
      next if idx < 3
      items = item.split(' ')
      domU = items[1]
      domus << domU
    end

    domus
  end
  
  def self.total_vcpus
    _, res = system('virsh nodeinfo')
    res =~ /CPU\(s\): +(\d)/
    $1.to_i
  end
  
  def self.total_mhz
    _, res = system('virsh nodeinfo')
    res =~ /CPU frequency: +(\d+) MHz/
    vcpu_mhz = $1.to_i
    total_vcpus * vcpu_mhz
  end
  
  def self.total_memory
    _, res = system('virsh nodeinfo')
    res =~ /Memory size: +(\d+) kB/
    $1.to_i / 1024
  end

  def self.free_memory
    _, res = systemu 'virsh freecell'
    res.split(' ')[1].to_i / 1024
  end

  def self.get_cpu(domU)
    _, res = systemu "virsh schedinfo #{domU}"
    list = res.split("\n")
    weight = list[1].split(" ")[2].to_i
    cap = list[2].split(" ")[2].to_i

    [weight, cap]
  end

  def self.set_cpu(domU, weight, cap)
    cmd = "xm sched-credit -d #{domU} -w #{weight} -c #{cap}"
    puts cmd
    _, res = systemu(cmd)
  end

  def self.get_memory(domU)
    _, res = systemu "xm list"
    list = res.split("\n")
    list.each do |vm|
      uid = vm.split(' ')[0]
      mem = vm.split(' ')[2]

      return mem.to_i if uid == domU
    end
  end

  def self.start(xml_config)
    cmd = "virsh create #{xml_config}"
    puts cmd
    _, out, err = systemu(cmd)
    puts out
    puts err
    
    err.blank?
  end

  def self.restart(domU)
    _, res = systemu "virsh reboot #{domU}"
  end

  def self.stop(domU)
    cmd = "virsh destroy #{domU}"
    puts cmd
    _, out, err = systemu(cmd)
    puts out
    puts err
    
    err.blank?
  end
  
  def self.attach_disk(domU, disk_number, block_device)
    cmd = "virsh attach-disk #{domU} /dev/md#{disk_number} #{block_device}"
    puts cmd
    _, res, err = systemu(cmd)
    err.blank?
  end
  
  def self.detach_disk(domU, block_device)
    cmd = "virsh detach-disk #{domU} #{block_device}"
    puts cmd
    _, res, err = systemu(cmd)
    err.blank?
  end
end

class DomU
  attr_accessor :id
  attr_accessor :name
  attr_accessor :mem
  attr_accessor :vcpus
  attr_accessor :disks
  attr_accessor :cpu_weight
  attr_accessor :cpu_cap
  attr_accessor :eth0_mac
  attr_accessor :eth1_mac
  attr_accessor :vnc_port

  def initialize(id, name, mem, vcpus, disks, cpu_weight, cpu_cap)
    @id = id
    @name = name
    @mem = mem
    @vcpus = vcpus
    @disks = disks
    @cpu_weight = cpu_weight
    @cpa_cap = cpu_cap
    @eth0_mac = "00:16:3e:1b:00:#{@id.to_s(16)}"
    @eth1_mac = "00:16:3e:1b:a0:#{@id.to_s(16)}"
    @vnc_port = 5900 + @id
  end

  def to_xml
    template_file = File.open(File.join(AGENT_ROOT, 'domU.xml'))
    template = template_file.read()
    template_file.close()

    xml = ERB.new(template)
    xml.result(binding)
  end
end
