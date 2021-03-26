const sendTransaction = (params) =>
  window.ethereum.request({ method: "eth_sendTransaction", params: [params] });

// https://eth.wiki/json-rpc/API#eth_accounts
const getAccounts = () => window.ethereum.request({ method: "eth_accounts" });

// https://docs.metamask.io/guide/rpc-api.html#eth-requestaccounts
const requestAccounts = () =>
  window.ethereum.request({ method: "eth_requestAccounts" });

// https://docs.metamask.io/guide/rpc-api.html#wallet-addethereumchain
const xDaiImport = () =>
  window.ethereum.request({
    method: "wallet_addEthereumChain",
    params: [
      // https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
      {
        chainId: "0x64",
        chainName: "xDai Chain",
        rpcUrls: ["https://rpc.xdaichain.com"],
        blockExplorerUrls: ["https://blockscout.com/xdai/mainnet"],
        nativeCurrency: {
          name: "xDAI",
          symbol: "xDAI",
          decimals: 18,
        },
      },
    ],
  });

const getBalance = (address) =>
  window.ethereum.request({
    method: "eth_getBalance",
    params: [address, "latest"],
  });

const getWallet = async (address) => {
  const balance = await getBalance(address);

  const network = await window.ethereum.request({
    method: "net_version",
  });

  return { address, balance, network };
};

const handleWalletEvents = (port) => {
  window.ethereum.on("chainChanged", (_chain) =>
    (async () => {
      const [account] = await getAccounts();

      const wallet = account ? await getWallet(account) : null;

      port(wallet);
    })().catch((e) => {
      console.error("chainChanged", e);
      port(e);
    })
  );

  window.ethereum.on("accountsChanged", ([account]) =>
    (async () => {
      const wallet = account ? await getWallet(account) : null;

      port(wallet);
    })().catch((e) => {
      console.error("accountsChanged", e);
      port(e);
    })
  );

  window.ethereum.on("disconnect", (message) =>
    (async () => {
      console.log(message);

      const [account] = await getAccounts();

      const wallet = account ? await getWallet(account) : null;

      port(wallet);
    })().catch((e) => {
      console.error("disconnect", e);
      port(e);
    })
  );
};

module.exports = {
  getAccounts,
  getWallet,
  getBalance,
  requestAccounts,
  handleWalletEvents,
  xDaiImport,
  sendTransaction,
};
