require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  subject { Item.new(name: name, sell_in: sell_in, quality: quality) }

  let(:items) { [subject] }

  describe '#update_quality' do
    let(:sell_in) { 0 }
    let(:quality) { 0 }

    before do
      GildedRose
      .new(items: items)
      .update_quality
    end

    shared_examples :does_not_change_the_name do
      describe '#items' do
        describe '#first' do
          describe '#name' do
            it { expect(subject.name).to eq(name) }
          end
        end
      end
    end

    context 'when #name is any_other' do
      let(:name) { 'any_other' }

      it_behaves_like :does_not_change_the_name

      context 'when sell_in is negative' do
        let(:sell_in) { -42 }

        it { expect(subject.sell_in).to eq(-43) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(48) }
        end
      end

      context 'when sell_in is 4' do
        let(:sell_in) { 4 }

        it { expect(subject.sell_in).to eq(3) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(49) }
        end
      end

      context 'when sell_in is 8' do
        let(:sell_in) { 8 }

        it { expect(subject.sell_in).to eq(7) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(49) }
        end
      end

      context 'when sell_in is 12' do
        let(:sell_in) { 12 }

        it { expect(subject.sell_in).to eq(11) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(49) }
        end
      end
    end

    context 'when #name is Aged Brie' do
      let(:name) { 'Aged Brie' }

      it_behaves_like :does_not_change_the_name

      context 'when sell_in is negative' do
        let(:sell_in) { -42 }

        it { expect(subject.sell_in).to eq(-43) }
        it { expect(subject.quality).to eq(2) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 4' do
        let(:sell_in) { 4 }

        it { expect(subject.sell_in).to eq(3) }
        it { expect(subject.quality).to eq(1) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 8' do
        let(:sell_in) { 8 }

        it { expect(subject.sell_in).to eq(7) }
        it { expect(subject.quality).to eq(1) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 12' do
        let(:sell_in) { 12 }

        it { expect(subject.sell_in).to eq(11) }
        it { expect(subject.quality).to eq(1) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end
    end

    context 'when #name is Sulfuras, Hand of Ragnaros' do
      let(:name) { 'Sulfuras, Hand of Ragnaros' }

      it_behaves_like :does_not_change_the_name

      context 'when sell_in is negative' do
        let(:sell_in) { -42 }

        it { expect(subject.sell_in).to eq(-42) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 4' do
        let(:sell_in) { 4 }

        it { expect(subject.sell_in).to eq(4) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 8' do
        let(:sell_in) { 8 }

        it { expect(subject.sell_in).to eq(8) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 12' do
        let(:sell_in) { 12 }

        it { expect(subject.sell_in).to eq(12) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end
    end

    context 'when #name is Backstage passes to a TAFKAL80ETC concert' do
      let(:name) { 'Backstage passes to a TAFKAL80ETC concert' }

      it_behaves_like :does_not_change_the_name

      context 'when sell_in is negative' do
        let(:sell_in) { -42 }

        it { expect(subject.sell_in).to eq(-43) }
        it { expect(subject.quality).to eq(0) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(0) }
        end
      end

      context 'when sell_in is 4' do
        let(:sell_in) { 4 }

        it { expect(subject.sell_in).to eq(3) }
        it { expect(subject.quality).to eq(3) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 8' do
        let(:sell_in) { 8 }

        it { expect(subject.sell_in).to eq(7) }
        it { expect(subject.quality).to eq(2) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end

      context 'when sell_in is 12' do
        let(:sell_in) { 12 }

        it { expect(subject.sell_in).to eq(11) }
        it { expect(subject.quality).to eq(1) }

        context 'when quality is >= 50' do
          let(:quality) { 50 }
          it { expect(subject.quality).to eq(50) }
        end
      end
    end
  end
end
