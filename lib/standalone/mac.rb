class MAC
  def self.generate(cube_number, vif_number, vps_id)
    last = ("%4s" % vps_id.to_s(16)).gsub(' ', '0')
    "00:16:3b:%s%s:%s:%s" % [cube_number.to_s(16), vif_number.to_s(16), last[0..1], last[2..3]]
  end
end