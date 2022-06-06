import React from 'react';
import ReactDOM from 'react-dom/client';
import App from './components/App';
import StoreProvider from './components/StoreProvider';

const root = ReactDOM.createRoot(
  document.getElementById('root') as HTMLElement
);
root.render(
  <React.StrictMode>
    <StoreProvider>
      <App />
    </StoreProvider>
  </React.StrictMode>
);
