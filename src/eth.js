const txSentry = (fromElm, toElm) => {
  fromElm.subscribe((txData) => {
    window.ethereum
      .request({ method: "eth_sendTransaction", params: [txData.txParams] })
      .then((r) => toElm.send({ ref: txData.ref, txHash: r }))
      .catch((e) => {
        console.error("send txn", e);
        toElm.send({ ref: txData.ref, txHash: null });
      });
  });
};

// https://eth.wiki/json-rpc/API#eth_accounts
const getAccounts = () => window.ethereum.request({ method: "eth_accounts" });

// https://docs.metamask.io/guide/rpc-api.html#eth-requestaccounts
const requestAccounts = () =>
  window.ethereum.request({ method: "eth_requestAccounts" });

const getWallet = async (address) => {
  const balance = await window.ethereum.request({
    method: "eth_getBalance",
    params: [address, "latest"],
  });

  const network = await window.ethereum.request({
    method: "net_version",
  });

  return { address, balance, network };
};

module.exports = {
  getAccounts,
  getWallet,
  txSentry,
  requestAccounts,
};
