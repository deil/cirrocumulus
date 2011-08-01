require 'rubygems'
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

  def self.find_by_disk_number(disk_number)
    fact = KnownFact.current.find_by_key('vd' + disk_number.to_s)
    return nil unless fact
    json = JSON.parse(fact.value)
    VirtualDisk.new(disk_number, json['size'])
  end
  
  def save
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s)
    fact = KnownFact.new(:key => 'vd' + @disk_number.to_s, :is_active => 1) unless fact
    fact.value = self.to_json
    fact.save
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
  
  def save
    fact = KnownFact.current.find_by_key('vd' + @disk_number.to_s + '-state')
    fact = KnownFact.new(:key => 'vd' + @disk_number.to_s + '-state', :is_active => 1) unless fact
    fact.value = @is_up ? 'up' : 'down'
    fact.save
  end
  
end

class ExportState < ActiveRecord::Base
end

ActiveRecord::Base.establish_connection(
  :adapter => 'sqlite3',
  :database => "#{AGENT_ROOT}/storage.sqlite"
)

class AgentState
  def self.export_should_be_up?(disk_number)
    state = ExportState.find_by_disk_number(disk_number)
    return state && state.is_up? ? true : false
  end
  
  def self.update_export_state(disk_number, is_up)
    state = ExportState.find_by_disk_number(disk_number)
    state = ExportState.new(:disk_number => disk_number) if state.nil?
    state.is_up = is_up
    state.save
  end
end
