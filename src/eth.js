const txSentry = (fromElm, toElm) => {
  fromElm.subscribe((txData) => {
    window.ethereum
      .request({ method: "eth_sendTransaction", params: [txData.txParams] })
      .then((r) => toElm.send({ ref: txData.ref, txHash: r }))
      .catch((e) => {
        console.log(e);
        toElm.send({ ref: txData.ref, txHash: null });
      });
  });
};

const enable = () => window.ethereum.request({ method: "eth_requestAccounts" });

module.exports = {
  txSentry,
  enable,
};
