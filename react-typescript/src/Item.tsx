import { useEffect, useState } from "react";

function calculateQuality(name:string, quality: number, sellIn: number,isConjured: boolean): number {
  const sellInAmount = sellIn;

  let calculatedQuality = quality;
  let degradeRate = 1;
  let qualityIdentifier = 1;

  if(sellInAmount <= 0) {
    return calculatedQuality;
  }

  if(name.includes("Sulfuras")) {
    return 80;
  };

  if(calculatedQuality >= 50) {
    return 50;
  } else if(calculatedQuality <= 0) {
    return 0;
  }

  if(isConjured) {
    degradeRate = 2;
  }

  if(name.includes('Aged Brie')) {
    qualityIdentifier = -1;
  }

  if(name.includes('Backstage passes')) {
    if(sellInAmount <= 10 && sellInAmount > 5) {
      qualityIdentifier = -3;
    } else if(sellInAmount <= 5 && sellInAmount > 0) {
      qualityIdentifier = -2;
    } else if(sellInAmount <= 0) {
      qualityIdentifier = 0;
      degradeRate = 0;
    }
  }

  return degradeRate * calculatedQuality - qualityIdentifier;  
};

function calculateSellIn(name:string, sellIn: number): number {
  let sellInAmount = sellIn;

  if(name.includes("Sulfuras")) {
    return sellInAmount;
  };

  if(sellInAmount <= 0) {
    return 0;
  }

  return sellIn - 1;
};

type ItemProps = {
    name: string,
    quality: number,
    sellIn: number,
    isConjured: boolean
};

const Item = (props: ItemProps) => {
    const [name, setName] = useState(props.name);
    useEffect(() => {
        setName(props.name);
    }, [props.name]);

    const [quality, setQuality] = useState(props.quality);
    useEffect(() => {
        setQuality(props.quality);
    }, [props.quality]);

    const [sellIn, setSellIn] = useState(props.sellIn);
    useEffect(() => {
        setSellIn(props.sellIn);
    }, [props.sellIn]);

    const [isConjured, setIsConjured] = useState(props.isConjured);
    useEffect(() => {
        setIsConjured(props.isConjured);
    }, [props.isConjured]);

    return (
        <div>
          <p color={isConjured ? 'Red' : 'Green'}>{name}</p>

          <p>{quality}</p>

          <p>{sellIn}</p>
          <br></br>
          <button onClick={() => { 
            setQuality(calculateQuality(name, sellIn, quality, isConjured));
            setSellIn(calculateSellIn(name, sellIn));
            }}>
            Purchase
          </button>
          <br></br>
        </div>
    );
};

export default Item;