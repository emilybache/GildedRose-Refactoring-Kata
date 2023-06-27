require_relative '../gilded_rose'

describe GildedRose do
  let(:quality) { 3 }
  let(:sell_in) { 3 }
  let(:item_name) { 'Elixir of the Mongoose' }
  let(:item) { Item.new(item_name, sell_in, quality) }
  let(:items) { [item] }

  subject { GildedRose.new(items) }

  describe '#update_quality' do
    context "when item is regular like e.g. 'Elixir of the Mongoose'" do
      context 'when sell by date has not passed' do
        it 'lowers quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 2
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 2
        end
      end

      context 'when 1 day left' do
        let(:sell_in) { 1 }

        it 'lowers quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 2
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 0
        end
      end

      context 'when sell by date has passed' do
        let(:sell_in) { 0 }

        it 'lowers quality by 2' do
          subject.update_quality
          expect(items[0].quality).to eq 1
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq -1
        end
      end

      context 'in all cases' do
        let(:quality) { 0 }

        it 'does never lower the quality below 0' do
          subject.update_quality
          expect(items[0].quality).to eq 0
        end
      end
    end

    context "when item name is 'Sulfuras, Hand of Ragnaros'" do
      let(:item_name) { 'Sulfuras, Hand of Ragnaros' }
      let(:quality) { 80 }

      context 'when sell by date has not passed' do
        it 'does not lower quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 80
        end

        it 'does not lower sell_in' do
          subject.update_quality
          expect(items[0].sell_in).to eq 3
        end
      end

      context 'when 1 day left' do
        let(:sell_in) { 1 }

        it 'does not lower quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 80
        end

        it 'does not lower sell_in' do
          subject.update_quality
          expect(items[0].sell_in).to eq 1
        end
      end

      context 'when sell by date has passed' do
        let(:sell_in) { 0 }

        it 'does not lower quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 80
        end

        it 'does not lower sell_in' do
          subject.update_quality
          expect(items[0].sell_in).to eq 0
        end
      end
    end

    context "when item name is 'Aged Brie'" do
      let(:item_name) { 'Aged Brie' }

      context 'when sell by date has not passed' do
        it 'increases quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 4
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 2
        end
      end

      context 'when sell by date has passed' do
        let(:sell_in) { 0 }

        it 'increases quality by 2' do
          subject.update_quality
          expect(items[0].quality).to eq 5
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq -1
        end
      end

      context 'when 1 day left' do
        let(:sell_in) { 1 }

        it 'increases quality by 1' do
          subject.update_quality
          expect(items[0].quality).to eq 4
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 0
        end
      end

      context 'in all cases' do
        let(:quality) { 50 }

        it 'does never raise the quality above 50' do
          subject.update_quality
          expect(items[0].quality).to eq 50
        end
      end
    end

    context "when item name is 'Backstage passes to a TAFKAL80ETC concert'" do
      let(:item_name) { 'Backstage passes to a TAFKAL80ETC concert' }

      context 'when sell by date has not passed' do
        context 'when 10 or less days remaining' do
          let(:sell_in) { 10 }

          it 'increases quality by 2' do
            subject.update_quality
            expect(items[0].quality).to eq 5
          end

          it 'lowers sell_in by 1' do
            subject.update_quality
            expect(items[0].sell_in).to eq 9
          end
        end

        context 'when 5 or less days remaining' do
          let(:sell_in) { 5 }
          it 'increases quality by 3' do
            subject.update_quality
            expect(items[0].quality).to eq 6
          end

          it 'lowers sell_in by 1' do
            subject.update_quality
            expect(items[0].sell_in).to eq 4
          end
        end
      end

      context 'when 1 day left' do
        let(:sell_in) { 1 }

        it 'increases quality by 3' do
          subject.update_quality
          expect(items[0].quality).to eq 6
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 0
        end
      end

      context 'when sell by date has passed' do
        let(:sell_in) { 0 }

        it 'sets the quality to 0' do
          subject.update_quality
          expect(items[0].quality).to eq 0
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq -1
        end
      end

      context 'in all cases' do
        let(:quality) { 50 }

        it 'does never raise the quality above 50' do
          subject.update_quality
          expect(items[0].quality).to eq 50
        end
      end
    end

    context "when item name is 'Conjured Mana Cake'" do
      let(:item_name) { 'Conjured Mana Cake' }

      context 'when sell by date has not passed' do
        it 'lowers quality by 2' do
          subject.update_quality
          expect(items[0].quality).to eq 1
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 2
        end
      end

      context 'when 1 day left' do
        let(:sell_in) { 1 }

        it 'lowers quality by 2' do
          subject.update_quality
          expect(items[0].quality).to eq 1
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq 0
        end
      end

      context 'when sell by date has passed' do
        let(:sell_in) { 0 }
        let(:quality) { 6 }

        it 'lowers quality by 4' do
          subject.update_quality
          expect(items[0].quality).to eq 2
        end

        it 'lowers sell_in by 1' do
          subject.update_quality
          expect(items[0].sell_in).to eq -1
        end
      end

      context 'in all cases' do
        let(:quality) { 0 }

        it 'does never lower the quality below 0' do
          subject.update_quality
          expect(items[0].quality).to eq 0
        end
      end
    end
  end
end
