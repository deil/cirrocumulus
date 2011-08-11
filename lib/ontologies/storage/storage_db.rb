require 'activesupport'
require 'activerecord'

class KnownFact < ActiveRecord::Base
  named_scope :current, :conditions => {:is_active => 1}
end

class VirtualDisk
  attr_reader :disk_number
  attr_accessor :size
  
  def initialize(disk_number, size)
    @disk_number = disk_number
    @size = size
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
    VirtualDisk.new(disk_number, json['size'])
  end
  
  def save(origin = nil, agent = nil)
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s)
    fact = KnownFact.new(:key => 'vd' + @disk_number.to_s, :is_active => 1) unless fact
    fact.value = self.to_json
    fact.origin = origin
    fact.agent = agent
    fact.save
  end

  def delete
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

ActiveRecord::Base.establish_connection(
  :adapter => 'sqlite3',
  :database => "#{AGENT_ROOT}/ontologies/storage/storage.sqlite"
)

class AgentState
  def self.export_should_be_up?(disk_number)
    state = VirtualDiskState.find_by_disk_number(disk_number)
    return state && state.is_up == true ? true : false
  end
  
  def self.update_export_state(disk_number, is_up)
    state = VirtualDiskState.find_by_disk_number(disk_number)
    state = VirtualDiskState.new(disk_number, is_up) unless state
    state.is_up = is_up
    state.save
  end
end
