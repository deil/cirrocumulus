require_relative '../lib/cirrocumulus/facts'

class Storage < KnowledgeClass
  klass :storage
  id :number
  property :state
  property :capacity
end

class StorageWithoutId < KnowledgeClass
  klass :storage
  property :state
  property :capacity
end

describe KnowledgeClass do
  describe '+from_fact' do
    it 'correctly initializes from fact standard representation' do
      fact = [:storage, 2, :state, :offline, :capacity, 100]
      s = Storage.from_fact(fact)

      s.should be_instance_of Storage
      s.number.should == 2
      s.state.should == :offline
      s.capacity.should == 100
    end
  end

  describe '+to_template' do
    it 'returns fact template' do
      Storage.to_template.should == [:storage, :NUMBER, :state, :STATE, :capacity, :CAPACITY]
    end

    it 'works correctly without primary key' do
      StorageWithoutId.to_template.should == [:storage, :state, :STATE, :capacity, :CAPACITY]
    end
  end

  describe '+new' do
    it 'correctly initializes objects fields' do
      s = Storage.new(:state => :online)
      s.state.should == :online
      s.capacity.should == nil
      s.number.should == nil
    end
  end

  describe '#to_template' do
    it 'serializes instance to fact template' do
      s = Storage.new
      s.number = 1
      s.state = :online
      s.capacity = 100

      s.to_template.should == [:storage, 1, :state, :online, :capacity, 100]
    end

    it 'works correctly without primary key' do
      s = StorageWithoutId.new :state => :online, :capacity => 100
      s.to_template.should == [:storage, :state, :online, :capacity, 100]
    end
  end

  describe '#to_params' do
    it 'should generate correct pattern for rule condition' do
      s = Storage.new :state => :online
      s.to_params.should == [:storage, :NUMBER, :state, :online, :capacity, :CAPACITY]
    end

    it 'works correctly without primary key definition' do
      s = StorageWithoutId.new :state => :online
      s.to_params.should == [:storage, :state, :online, :capacity, :CAPACITY]
    end
  end

  describe '#to_fact' do
    it 'correctly serializes instance to fact standard representation' do
      s = Storage.new
      s.number = 1
      s.state = :online
      s.capacity = 100

      s.to_fact.should == [:storage, 1, :state, :online, :capacity, 100]
    end
  end
end
