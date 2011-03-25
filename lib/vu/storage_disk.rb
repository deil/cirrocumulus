class StorageDisk < ActiveRecord::Base
	def current
		StorageDiskHistory.first(:conditions => {:storage_disk_id => disk_number}, :order => 'timestamp desc')
	end

	def history
		StorageDiskHistory.all(:conditions => {:storage_disk_id => disk_number}, :order => 'timestamp desc')
	end
end
