class VpsConfigurationHistory < ActiveRecord::Base
	def vcpus
		if ram >= 8192
			return 3
		elsif ram >= 4096
			return 2
		else
			return 1
		end
	end

	def cpu_cap
		0
	end

	def cpu_weight
		ram
	end
end
