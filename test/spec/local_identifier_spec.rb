require 'bundler/setup'
require_relative '../../lib/cirrocumulus'

describe LocalIdentifier do
  it 'two instances of class should be equal' do
    id1 = LocalIdentifier.new('test')
    id2 = LocalIdentifier.new('test')
    (id1 == id2).should be_true
  end
  
  it 'two different instances of class should not be equal' do
    id1 = LocalIdentifier.new('test1')
    id2 = LocalIdentifier.new('test2')
    (id1 == id2).should be_false
  end
end
