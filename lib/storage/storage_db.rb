require 'rubygems'
require 'activesupport'
require 'activerecord'

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
