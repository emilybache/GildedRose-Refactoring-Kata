require 'rspec'

require File.join(File.dirname(__FILE__), 'gilded_rose')

describe GildedRose do
  it "does not change the name" do
    items = [Item.new("foo", 0, 0)]
    GildedRose.new(items).update_quality()
    expect(items[0].name).to eq "fixme"
  end

  context 'para items normales' do
    before do
      ## Arrange
      @items = [Item.new("foo", 10, 2)]
      @gilded = GildedRose.new(@items)
    end
    

    it 'decrementa la calidad del item' do
      #Act
      @gilded.update_quality()
      
      #Assert
      expect(@items[0].quality).to eq 1
    end

    it 'decrementa los dias para vender el item' do    
      #Act
      @gilded.update_quality()
      
      #Assert
      expect(@items[0].sell_in).to eq 9
    end

    it 'decrementa la calidad del item al doble de velocidad cuando ya no quedan dias para venderlo' do
      @items[0].sell_in = 0
      @items[0].quality = 20
      
      @gilded.update_quality()

      expect(@items[0].quality).to eq 18
    end

    it 'no decrementa la calidad del item a negativo' do
      @items[0].quality = 0
      
      @gilded.update_quality()

      expect(@items[0].quality).to eq 0
    end
  end

  context 'para items Aged Brie' do
    it 'incrementa su calidad'

    it 'incrementa su calidad en 2 cuando ya no quedan dias de venta'

    it 'no incrementa su calidad por sobre 50'
  end

  context 'para items Sulfuras' do
    it 'no cambia su calidad'

    it 'no cambia los dias para venderlo'
  end

  context 'para items Backstage Pass' do
    it 'incrementa su calidad si quedan más de 10 días para venderlo'

    it 'incrementa su calidad en 2 si quedan 10 dias o menos para venderlo'

    it 'incrementa su calidad en 3 si quedan 5 dias o menos para venderlo'

    it 'decrementa su calidad a 0 si ya no quedan dias para venderlo'
  end

  context 'para items conjurados' do
    it 'decrementa su calidad en 2'
  end
end
