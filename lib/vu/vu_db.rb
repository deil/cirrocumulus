require 'rubygems'
require 'activesupport'
require 'activerecord'
require 'guid'

class Vps < ActiveRecord::Base

  def disks
    Disk.all(:conditions => {:vps_uid => uid}, :order => 'block_device asc')
  end
  
  def attach_disk(disk_number, block_device)
    Disk.create(:disk_number => disk_number, :vps_uid => uid, :block_device => block_device)
  end
  
  def detach_disk(disk_number)
    disk = Disk.find_by_disk_number_and_vps_uid(disk_number, uid)
    disk.delete if disk
  end

  def self.create_vps(ram, cpu = 0, hvm = false)
    vps = self.new(:ram => ram, :cpu => cpu, :hvm => hvm)
    vps.uid = Guid.new.to_s.gsub('-', '')
    vps.save
    vps
  end
end

class Disk < ActiveRecord::Base
end

AGENT_ROOT = "." if !defined?(AGENT_ROOT)

ActiveRecord::Base.establish_connection(
  :adapter => 'sqlite3',
  :database => "#{AGENT_ROOT}/vu.sqlite"
)

class AgentState
end
