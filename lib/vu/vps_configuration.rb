class VpsConfiguration < ActiveRecord::Base
	has_many :storage_disks_vps_configurations
	has_many :storage_disks, :through => :storage_disks_vps_configurations

  def current
    VpsConfigurationHistory.first(:conditions => {:vps_id => vps_id}, :order => 'timestamp desc')
  end
  
  def history
    VpsConfigurationHistory.all(:conditions => {:vps_id => vps_id}, :order => 'timestamp desc')
  end

  def to_s
    vps_id
  end

	def self.active
		self.all(:conditions => {:is_active => true})
	end
end
