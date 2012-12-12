require_relative '../lib/cirrocumulus/facts'
require_relative '../lib/cirrocumulus/pattern_matching'

describe FactsDatabase do
  it 'should be empty at first' do
    db = FactsDatabase.new
    db.enumerate.size.should == 0
  end

  it 'should enumerate added fact' do
    db = FactsDatabase.new
    db.add [:test1]
    db.enumerate.size.should == 1
    db.enumerate.first.data.should == [:test1]
  end

  it 'should hide removed fact' do
    db = FactsDatabase.new
    db.add [:test1]
    db.add ['test2']
    db.enumerate.size.should == 2
    db.remove ['test2']
    db.enumerate.size.should == 1
    db.enumerate.first.data.should == [:test1]
  end

  it 'should emit immutable enumerator' do
    db = FactsDatabase.new
    db.add [:test1]
    db.add ['test2']

    db.enumerate.size.should == 2

    internals = db.enumerate
    internals.clear

    db.enumerate.size.should == 2
  end
end
