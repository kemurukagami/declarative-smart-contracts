contract TokenPartition {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesByPartitionTuple {
    uint m;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>mapping(uint=>TotalOutTuple)) totalOut;
  OwnerTuple owner;
  mapping(uint=>TotalSupplyByPartitionTuple) totalSupplyByPartition;
  mapping(address=>mapping(uint=>TotalBurnTuple)) totalBurn;
  AllMintTuple allMint;
  mapping(uint=>TotalBalancesByPartitionTuple) totalBalancesByPartition;
  mapping(address=>mapping(uint=>TotalInTuple)) totalIn;
  mapping(address=>mapping(uint=>TotalMintTuple)) totalMint;
  AllBurnTuple allBurn;
  event TransferByPartition(address from,address to,uint q,uint amount);
  event IssueByPartition(address p,uint q,uint amount);
  event RedeemByPartition(address p,uint q,uint amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r3();
    updateOwnerOnInsertConstructor_r12();
  }
  function redeemByPartition(address p,uint q,uint amount) public    {
      bool r10 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r10(p,q,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function issueByPartition(address p,uint q,uint amount) public    {
      bool r1 = updateIssueByPartitionOnInsertRecv_issueByPartition_r1(p,q,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function getBalanceOfByPartition(address p,uint q) public view  returns (uint) {
      uint n = balanceOfByPartition(p,q);
      return n;
  }
  function getTotalSupplyByPartition(uint q) public view  returns (uint) {
      uint n = totalSupplyByPartition[q].n;
      return n;
  }
  function transferByPartition(address from,address to,uint q,uint amount) public    {
      bool r9 = updateTransferByPartitionOnInsertRecv_transferByPartition_r9(from,to,q,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r14(address p,uint q,int m) private    {
      // Empty()
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r14(address p,uint q,int o) private    {
      // Empty()
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r7(int b) private    {
      // Empty()
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalInOnInsertTransferByPartition_r17(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r14(p,q,delta0);
      totalIn[p][q].n += n;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r4(uint q,int r) private    {
      int _delta = int(-r);
      uint newValue = updateuintByint(totalSupplyByPartition[q].n,_delta);
      totalSupplyByPartition[q].n = newValue;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r1(address p,uint q,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateIssueTotalByPartitionOnInsertIssueByPartition_r5(q,n);
          updateTotalMintOnInsertIssueByPartition_r8(p,q,n);
          updateAllMintOnInsertIssueByPartition_r0(n);
          emit IssueByPartition(p,q,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r16(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r14(p,q,delta0);
      totalBurn[p][q].n += n;
  }
  function updateAllMintOnInsertIssueByPartition_r0(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r7(delta0);
      allMint.n += n;
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r9(address s,address r,uint q,uint n) private   returns (bool) {
      uint m = balanceOfByPartition(s,q);
      if(n<=m) {
        updateTotalInOnInsertTransferByPartition_r17(r,q,n);
        updateTotalOutOnInsertTransferByPartition_r6(s,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r14(address p,uint q,int i) private    {
      // Empty()
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r15(uint q,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r4(q,delta0);
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r4(uint q,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(totalSupplyByPartition[q].n,_delta);
      totalSupplyByPartition[q].n = newValue;
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r14(address p,uint q,int n) private    {
      // Empty()
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r5(uint q,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r4(q,delta0);
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r7(int m) private    {
      // Empty()
  }
  function balanceOfByPartition(address p,uint q) private view  returns (uint) {
      uint i = totalIn[p][q].n;
      uint o = totalOut[p][q].n;
      uint m = totalBurn[p][q].n;
      uint n = totalMint[p][q].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateTotalMintOnInsertIssueByPartition_r8(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r14(p,q,delta0);
      totalMint[p][q].n += n;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r10(address p,uint q,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOfByPartition(p,q);
        if(p!=address(0) && n<=m) {
          updateAllBurnOnInsertRedeemByPartition_r11(n);
          updateTotalBurnOnInsertRedeemByPartition_r16(p,q,n);
          updateRedeemTotalByPartitionOnInsertRedeemByPartition_r15(q,n);
          emit RedeemByPartition(p,q,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransferByPartition_r6(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r14(p,q,delta0);
      totalOut[p][q].n += n;
  }
  function updateAllBurnOnInsertRedeemByPartition_r11(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r7(delta0);
      allBurn.n += n;
  }
}