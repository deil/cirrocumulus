require 'bundler/setup'
require_relative '../../lib/cirrocumulus'

describe RemoteIdentifier do
  it 'two instances of class should be equal' do
    id1 = RemoteIdentifier.new('test')
    id2 = RemoteIdentifier.new('test')
    (id1 == id2).should be_true
  end
  
  it 'two different instances of class should not be equal' do
    id1 = RemoteIdentifier.new('test1')
    id2 = RemoteIdentifier.new('test2')
    (id1 == id2).should be_false
  end
end
