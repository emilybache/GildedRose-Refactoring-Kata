import { useEffect, useState } from "react";

function calculateQuality(quality: number, isConjured: boolean): number {
  const calculatedQuality = quality;
  const calculatedBias = isConjured ? 2 : 1;

  if(calculatedQuality > 50) {
    return 50;
  } else if(calculatedQuality < 0) {
    return 0;
  }

  

  return calculatedQuality * calculatedBias;  
};

function calculateSellIn(sellIn: number): number {
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
          <p color={isConjured ? 'Red' : 'Green'}>The name {name}</p>

          <p>The quality {quality}</p>

          <p>The sellIn {sellIn}</p>
          <br></br>
          <button onClick={() => { 
            setSellIn(calculateSellIn(sellIn));
            setQuality(calculateQuality(quality, isConjured));
            }}>
            Next Day
          </button>
          <br></br>
        </div>
    );
};

export default Item;