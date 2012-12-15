require_relative '../lib/cirrocumulus/pattern_matching'
require_relative '../lib/cirrocumulus/rule_queue'
require_relative '../lib/cirrocumulus/ontology'

describe RuleQueue do
  before :each do
    @q = RuleQueue.new
  end

  describe '#pop' do
    it 'returns nil if queue is empty' do
      @q.pop.should be_nil
    end
  end

  describe '#push' do
    it 'ignores duplicates' do
      @q.push MatchResult.new(RuleDescription.new('test1', [[:test1]], {}, nil))
      @q.push MatchResult.new(RuleDescription.new('test2', [[:test2]], {}, nil))
      @q.size.should == 2
      @q.push MatchResult.new(RuleDescription.new('test1', [[:test1]], {}, nil))
      @q.size.should == 2
    end
  end

  describe '#pop' do
    it 'returns instance of RunQueue::QueueEntry' do
      @q.push MatchResult.new(RuleDescription.new('test1', [[:test1]], {}, nil))
      @q.pop.should be_instance_of RuleQueue::QueueEntry
    end
  end

  it 'pushes items to the end, pops from beginning' do
    r1 = MatchResult.new(RuleDescription.new('test1', [[:test1]], {}, nil))
    r2 = MatchResult.new(RuleDescription.new('test2', [[:test2]], {}, nil))
    r3 = MatchResult.new(RuleDescription.new('test3', [[:test3]], {}, nil))
    @q.push r1
    @q.push r2
    @q.push r3

    @q.pop.run_data.should equal r1
    @q.pop.run_data.should equal r2
    @q.pop.run_data.should equal r3
  end
end
