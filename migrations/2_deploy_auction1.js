const Auction = artifacts.require("Auction");

// Use a real address from your local blockchain (Ganache) and a bidding time in seconds
const beneficiary = "0x1234567890123456789012345678901234567890"; // Replace with a real address
const biddingTime = 600; // e.g., 10 minutes

module.exports = function (deployer) {
  deployer.deploy(Auction, beneficiary, biddingTime);
};