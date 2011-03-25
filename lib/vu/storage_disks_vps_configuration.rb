class StorageDisksVpsConfiguration < ActiveRecord::Base
  belongs_to :storage_disk
  belongs_to :vps_configuration
 
  def block_device
    "xvd" + ("a".ord + priority).chr
  end
end
