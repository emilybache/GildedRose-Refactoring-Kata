import { motion } from 'framer-motion';
import styled from 'styled-components';
import useAllItems from '../hooks/useAllItems';
import useDayLoader from '../hooks/useDayLoader';

function TableHeader() {
    const Button = styled(motion.button)`
        border: 1px solid RGB(12, 16, 36);
        padding: 10px;
        border-radius: 5px;
        background-color: RGB(12, 16, 36);
        color: RGB(245, 245, 243);
    margin: 0 1%;
    height: 80%;
        `
    const Header = styled(motion.span)`
    display: flex;
    flex-flow: row wrap;
    justify-content: space-between;
    align-items: center;
        font-size: 1.5rem;
        font-weight: 300;
        padding: .5rem;
        border-bottom: 1px solid RGB(12, 16, 36);
        height: 10%;
    `
    const HeaderText = styled(motion.span)`
    display: flex;
    flex-flow: column no-wrap;
    justify-content: center;
    align-items: center;
    font-size: 1.5rem;
    font-weight: 500;
    padding: .5rem;
    color: RGB(12, 16, 36);
    `

    const ItemsBadge = styled(motion.span)`
    align-self: center;
    font-size: 1rem;
    padding: .2rem;
    margin: 0 8%;
    background-color: RGB(149, 154, 163);
    color: RGB(245, 245, 243);
    border: 1px solid RGB(149, 154, 163);
    border-radius: 5px;
    `

    const { loadNextDay } = useDayLoader();
    const {items} = useAllItems();
    const count = items.length;
    return (
        <Header>
            <HeaderText>
                Items
                <ItemsBadge>
                    {count}
                </ItemsBadge>
            </HeaderText>
            <Button 
                onClick={loadNextDay}
                whileHover={{ scale: 1.1 }}
                whileTap={{ scale: 0.9 }}
            > 
                Next Day &#x2192;
            </Button>
        </Header>
    );
}

export default TableHeader;
