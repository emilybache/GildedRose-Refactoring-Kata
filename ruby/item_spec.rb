require File.join(File.dirname(__FILE__), 'item')

RSpec.describe Item do
  let(:item) do
    described_class.new(
      name: 'a',
      sell_in: 0,
      quality: 0,
    )
  end

  describe '#to_s' do
    it { expect(item.to_s).to eq('a, 0, 0') }
  end
end
