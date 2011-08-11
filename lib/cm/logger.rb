require 'log4r'
require 'log4r/configurator'

Log4r::Configurator.load_xml_file(File.join(AGENT_ROOT, 'config/log4r.xml'))
