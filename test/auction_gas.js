const Auction = artifacts.require("Auction");

function getRandomAccount(accounts, exclude = []) {
  // Return a random account not in the exclude list
  let filtered = accounts.filter(a => !exclude.includes(a));
  return filtered[Math.floor(Math.random() * filtered.length)];
}

contract("Auction Synthetic Workload", accounts => {
  let instance;

  beforeEach(async () => {
    instance = await Auction.new(accounts[0], 600);
  });

  it("synthetic workload for bid()", async () => {
    let gasUsages = [];
    for (let i = 0; i < 11; i++) {
      let tempInstance = await Auction.new(accounts[0], 600);
      let bidder = getRandomAccount(accounts, [accounts[0]]);
      let value = web3.utils.toWei((Math.floor(Math.random() * 10) + 1).toString(), "ether");
      let receipt = await tempInstance.bid({from: bidder, value});
      gasUsages.push(receipt.receipt.gasUsed);
    }
    // Discard the first (warm-up) and average the next 10
    let avgGas = gasUsages.slice(1).reduce((a, b) => a + b, 0) / 10;
    console.log("Average gas for bid() (synthetic):", avgGas);
  });

  it("synthetic workload for withdraw() (outbid bidders)", async () => {
    // Place 2 random bids so one can withdraw
    let bidders = [getRandomAccount(accounts, [accounts[0]]), getRandomAccount(accounts, [accounts[0]])];
    await instance.bid({from: bidders[0], value: web3.utils.toWei("1", "ether")});
    await instance.bid({from: bidders[1], value: web3.utils.toWei("2", "ether")});
    await increaseTime(601);
    await instance.endAuction({from: accounts[0]});

    let gasUsages = [];
    for (let i = 0; i < 11; i++) {
      // Deploy a new contract and repeat the setup for each run
      let tempInstance = await Auction.new(accounts[0], 600);
      let b1 = getRandomAccount(accounts, [accounts[0]]);
      let b2 = getRandomAccount(accounts, [accounts[0], b1]);
      await tempInstance.bid({from: b1, value: web3.utils.toWei("1", "ether")});
      await tempInstance.bid({from: b2, value: web3.utils.toWei("2", "ether")});
      await increaseTime(601);
      await tempInstance.endAuction({from: accounts[0]});
      let receipt = await tempInstance.withdraw({from: b1});
      gasUsages.push(receipt.receipt.gasUsed);
    }
    let avgGas = gasUsages.slice(1).reduce((a, b) => a + b, 0) / 10;
    console.log("Average gas for withdraw() (synthetic):", avgGas);
  });

  it("synthetic workload for endAuction()", async () => {
    let gasUsages = [];
    for (let i = 0; i < 11; i++) {
      let tempInstance = await Auction.new(accounts[0], 600);
      let bidder = getRandomAccount(accounts, [accounts[0]]);
      await tempInstance.bid({from: bidder, value: web3.utils.toWei("1", "ether")});
      await increaseTime(601);
      let receipt = await tempInstance.endAuction({from: accounts[0]});
      gasUsages.push(receipt.receipt.gasUsed);
    }
    let avgGas = gasUsages.slice(1).reduce((a, b) => a + b, 0) / 10;
    console.log("Average gas for endAuction() (synthetic):", avgGas);
  });
});

// Helper to advance time
function increaseTime(duration) {
  const id = Date.now();
  return new Promise((resolve, reject) => {
    web3.currentProvider.send(
      {
        jsonrpc: "2.0",
        method: "evm_increaseTime",
        params: [duration],
        id: id,
      },
      (err1) => {
        if (err1) return reject(err1);
        web3.currentProvider.send(
          {
            jsonrpc: "2.0",
            method: "evm_mine",
            id: id + 1,
          },
          (err2, res) => (err2 ? reject(err2) : resolve(res))
        );
      }
    );
  });
}