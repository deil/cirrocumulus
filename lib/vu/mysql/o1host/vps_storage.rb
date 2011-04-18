require "#{AGENT_ROOT}/dbconfig.rb"
require "#{AGENT_ROOT}/storage_disk_history.rb"
require "#{AGENT_ROOT}/storage_disk.rb"
require "#{AGENT_ROOT}/vps_configuration_history.rb"
require "#{AGENT_ROOT}/vps_configuration.rb"
require "#{AGENT_ROOT}/vps_state.rb"
require "#{AGENT_ROOT}/storage_disks_vps_configuration.rb"

Log4r::Logger['agent'].info "loading MYSQL-O1HOST VPS storage"

class VpsStorage
end
