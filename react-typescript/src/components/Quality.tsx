import { motion } from 'framer-motion';
import styled from "styled-components";

function qualityBackgroundColorHSL(value: number): string {
  const hue = Math.round((value / 50) * 360);
  return `hsl(${hue}, 100%, 80%)`;
}

function Quality(value: number, index: number): JSX.Element {
  const Quality = styled(motion.span)`
  role: 'region';
  font-size: 1rem;
  padding: .4rem;
  border-radius: 10px;
  color: RGB(12, 16, 36);
  background-color: ${qualityBackgroundColorHSL(value)};
`;

  return (
    <Quality 
      key={index}
    >
      {value}
    </Quality>
  );
}

export default Quality;