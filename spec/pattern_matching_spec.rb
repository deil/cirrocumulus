require_relative '../lib/cirrocumulus/facts'
require_relative '../lib/cirrocumulus/pattern_matching'
require_relative '../lib/cirrocumulus/ontology'

describe PatternMatcher do
  before do
    @facts = FactsDatabase.new
    @facts.add [:test1, :param, 'val']
    @facts.add [:test2, :param, 'world']
    @facts.add [:a, :param, 'ab']
    @facts.add [:a, :param, 'a']
    @facts.add [:b, :param, 'ab']
    @facts.add [:b, :param, 'b']
  end

  let(:matcher) { PatternMatcher.new(@facts.enumerate)}

  it 'should return empty result if nothing was found' do
    result = matcher.match [:i, :doesnt, :exist, :in, :database]
    result.size.should == 0
  end

  it 'should find matching fact' do
    result = matcher.match([:test1, :param, 'val'])
    result.size.should == 1
  end

  it 'should bind variables for found matches' do
    result = matcher.match [:test2, :param, :HELLO]
    result.first[:HELLO].should == 'world'
  end

  it 'should return nil if rule (from Ontology) matches nothing' do
    rule = RuleDescription.new('test rule', [ [:i, :doesnt, :match] ], {}, nil)
    matcher.matches?(rule).should == nil
  end

  it 'should return correct match result for matched rule (from Ontology)' do
    rule = RuleDescription.new('test rule', [ [:a, :param, :PARAM], [:b, :param, :PARAM] ], {}, nil)
    result = matcher.matches?(rule)

    result.size.should == 1

    result.first.should be_a(MatchResult)
    result.first.rule.should eq(rule)

    result.first.parameters.keys.size == 1
    result.first.parameters[:PARAM].should == 'ab'
  end

  describe 'for fact matching specified pattern' do
    it 'should correctly bind parameters' do
      params = matcher.pattern_matches?([:one, :simple, :fact], [:one, :VAL, :fact])
      params.size.should == 1
      params[:VAL].should == :simple
    end

    it 'should not override already bound parameters' do
      params = matcher.pattern_matches?([:one, :simple, :fact], [:one, :VAL, :fact], {VAL: :simple})
      params.size.should == 0
    end
  end

end
