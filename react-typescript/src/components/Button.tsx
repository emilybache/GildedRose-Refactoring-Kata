import { motion } from 'framer-motion';
import useDayLoader from '../hooks/useDayLoader';
import styled from 'styled-components';

//TODO: define styled components here

function Button() {
    const Button = styled(motion.button)`
        border: none;

        `

    const { loadNextDay } = useDayLoader();
    return (
        <Button onClick={loadNextDay}>Next Day</Button>
    );
}

export default Button;
