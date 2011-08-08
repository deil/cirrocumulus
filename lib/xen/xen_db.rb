require 'rubygems'
require 'activesupport'
require 'activerecord'

class KnownFact < ActiveRecord::Base
  named_scope :current, :conditions => {:is_active => 1}
end

class VirtualDisk
  attr_reader :disk_number
  
  def initialize(disk_number)
    @disk_number = disk_number
  end

  def self.all
    disks = []
    KnownFact.all(:conditions => ['key like "vd%%"']).each do |f|
      if f.key =~ /vd(\d+)$/
        disk_number = $1.to_i
        disks << self.find_by_disk_number(disk_number)
      end
    end

    disks
  end

  def self.find_by_disk_number(disk_number)
    fact = KnownFact.current.find_by_key('vd' + disk_number.to_s)
    return nil unless fact
    json = ActiveSupport::JSON.decode(fact.value)
    VirtualDisk.new(disk_number)
  end
  
  def save(origin = nil, agent = nil)
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s)
    fact = KnownFact.new(:key => 'vd' + @disk_number.to_s, :is_active => 1) unless fact
    fact.value = self.to_json
    fact.origin = origin
    fact.agent = agent
    fact.save
  end

  def delete()
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s)
    fact.update_attributes(:is_active => false) if fact
  end

end

class VirtualDiskState
  attr_reader :disk_number
  attr_accessor :is_up
  
  def initialize(disk_number, is_up)
    @disk_number = disk_number
    @is_up = is_up
  end

  def self.find_by_disk_number(disk_number)
    fact = KnownFact.current.find_by_key('vd' + disk_number.to_s + '-state')
    return nil unless fact
    VirtualDiskState.new(disk_number, fact.value == 'up')
  end
  
  def save(origin = nil, agent = nil)
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s + '-state')
    fact = KnownFact.new(:key => 'vd' + @disk_number.to_s + '-state', :is_active => 1) unless fact
    fact.value = @is_up ? 'up' : 'down'
    fact.origin = origin
    fact.agent = agent
    fact.save
  end

  def delete
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s + '-state')
    fact.update_attributes(:is_active => false) if fact
  end
  
end

class GuestState
  attr_reader :name
  attr_accessor :is_up
  
  def initialize(name, is_up)
    @name = name
    @is_up = is_up
  end
  
  def self.find_by_name(name)
    fact = KnownFact.current.find_by_key('domu_' + name + '-state')
    return nil unless fact
    GuestState.new(name, fact.value == 'running')
  end
  
  def save(origin = nil, agent = nil)
    fact = KnownFact.current.find_by_key('domu_' + name + '-state')
    fact = KnownFact.new(:key => 'domu_' + name + '-state', :is_active => 1) unless fact
    fact.value = @is_up ? 'running' : 'stopped'
    fact.origin = origin
    fact.agent = agent
    fact.save
  end
  
  def delete()
    fact = KnownFact.current.find_by_key('domu_' + name + '-state')
    fact.update_attributes(:is_active => false) if fact
  end
end

ActiveRecord::Base.establish_connection(
  :adapter => 'sqlite3',
  :database => "#{AGENT_ROOT}/xen.sqlite"
)

class AgentState
end
