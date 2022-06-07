import styled from "styled-components";
import {motion} from 'framer-motion';

function SellIn(value: number, index: number): JSX.Element {
  const SellIn = styled(motion.span)`
  role: 'region';
`;

  return (
                <SellIn 
                    key={index} 
                >
                  {Math.abs(value)}
                  { value < 0 ? " Days Passed": ""}
                </SellIn>
  );
}

export default SellIn;