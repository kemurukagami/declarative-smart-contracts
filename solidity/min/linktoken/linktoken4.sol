contract Linktoken {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct DecreaseAllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  mapping(address=>mapping(address=>DecreaseAllowanceTotalTuple)) decreaseAllowanceTotal;
  mapping(address=>TotalMintTuple) totalMint;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  TotalBalancesTuple totalBalances;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  AllBurnTuple allBurn;
  OwnerTuple owner;
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint n) public {
    updateBalanceOfOnInsertConstructor_r5(n);
    updateTotalMintOnInsertConstructor_r19(n);
    updateTotalBalancesOnInsertConstructor_r27(n);
    updateTotalSupplyOnInsertConstructor_r23(n);
    updateTotalInOnInsertConstructor_r13(n);
    updateOwnerOnInsertConstructor_r8();
  }
  function burn(address p,uint amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_approve_r26(s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r7 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r7(p,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function mint(address p,uint amount) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,uint n) public    {
      bool r12 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r12(p,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r11 = updateTransferFromOnInsertRecv_transferFrom_r11(from,to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf(p);
      return n;
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance(p,s);
      return n;
  }
  function transfer(address to,uint amount) public    {
      bool r2 = updateTransferOnInsertRecv_transfer_r2(to,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r28(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r11(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf(o);
      uint k = allowance(o,s);
      if(m>=n && k>=n && validRecipient(r)) {
        updateTransferOnInsertTransferFrom_r14(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r5(uint n) private    {
      address p = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r4(address p,int n) private    {
      // Empty()
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r6(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r24(o,s,delta0);
      decreaseAllowanceTotal[o][s].m += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      // Empty()
  }
  function updateTotalBalancesOnInsertConstructor_r27(uint n) private    {
      totalBalances = TotalBalancesTuple(n,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r4(address p,int i) private    {
      // Empty()
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransfer_r20(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(p,delta0);
      totalOut[p].n += n;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r12(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      if(validRecipient(s)) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r28(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(p,delta0);
      totalMint[p].n += n;
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      // Empty()
  }
  function updateTotalMintOnInsertConstructor_r19(uint n) private    {
      address s = msg.sender;
      totalMint[s] = TotalMintTuple(n,true);
  }
  function allowance(address o,address s) private view  returns (uint) {
      uint d = decreaseAllowanceTotal[o][s].m;
      uint l = spentTotal[o][s].m;
      uint m = allowanceTotal[o][s].m;
      uint n = (m-l)-d;
      return n;
  }
  function updateTotalSupplyOnInsertConstructor_r23(uint n) private    {
      // Empty()
  }
  function validRecipient(address p) private view  returns (bool) {
      address t = address(this);
      if(p!=t && p!=address(0)) {
        return true;
      }
      return false;
  }
  function balanceOf(address p) private view  returns (uint) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r24(address o,address s,int d) private    {
      // Empty()
  }
  function updateTotalBurnOnInsertBurn_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(p,delta0);
      totalBurn[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r25(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
      allBurn.n += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(address p,int m) private    {
      // Empty()
  }
  function updateTotalInOnInsertConstructor_r13(uint n) private    {
      address s = msg.sender;
      totalIn[s] = TotalInTuple(n,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateMintOnInsertRecv_mint_r9(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateAllMintOnInsertMint_r0(n);
          updateTotalMintOnInsertMint_r16(p,n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r7(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      if(m>=n && validRecipient(s)) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r6(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r26(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      if(validRecipient(s)) {
        uint d = n-m;
        updateAllowanceTotalOnInsertIncreaseAllowance_r28(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
      allMint.n += n;
  }
  function updateBurnOnInsertRecv_burn_r3(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf(p);
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r15(p,n);
          updateAllBurnOnInsertBurn_r25(n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalOut_r4(address p,int o) private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r2(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf(s);
      if(n<=m && validRecipient(r)) {
        updateTotalOutOnInsertTransfer_r20(s,n);
        updateTotalInOnInsertTransfer_r10(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(p,delta0);
      totalIn[p].n += n;
  }
  function updateTransferOnInsertTransferFrom_r14(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r20(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
}