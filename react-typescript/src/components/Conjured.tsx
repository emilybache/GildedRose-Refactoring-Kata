import styled from "styled-components";
import {motion} from 'framer-motion';

function Conjured(value: boolean, index: number): JSX.Element {
  const Conjured = styled(motion.span)`
  role: 'region';
`;

  return (
    <Conjured 
      key={index}
    >
      {value ? "✅" : "❌"}
    </Conjured>
  );
}

export default Conjured;