contract Wbtc {
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
  struct PendingOwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
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
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  PendingOwnerTuple pendingOwner;
  mapping(address=>TotalMintTuple) totalMint;
  AllMintTuple allMint;
  TotalBalancesTuple totalBalances;
  AllBurnTuple allBurn;
  OwnerTuple owner;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PausedTuple paused;
  event TransferOwnership(address p);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event Unpause(bool b);
  event ClaimOwnership(address p);
  event DecreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Pause(bool b);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r1();
    updateOwnerOnInsertConstructor_r30();
    updateTotalBalancesOnInsertConstructor_r35();
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r23 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r23(p,n);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function claimOwnership() public    {
      bool r15 = updateClaimOwnershipOnInsertRecv_claimOwnership_r15();
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint amount) public    {
      bool r17 = updateBurnOnInsertRecv_burn_r17(amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf(p);
      return n;
  }
  function transferOwnership(address p) public    {
      bool r29 = updateTransferOwnershipOnInsertRecv_transferOwnership_r29(p);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r33 = updateIncreaseAllowanceOnInsertRecv_approve_r33(s,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function increaseApproval(address p,uint n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r19(p,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r7 = updateUnpauseOnInsertRecv_unpause_r7();
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r10 = updatePauseOnInsertRecv_pause_r10();
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r2 = updateTransferOnInsertRecv_transfer_r2(to,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function reclaimToken() public    {
      bool r5 = updateReclaimTokenOnInsertRecv_reclaimToken_r5();
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r12(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
      totalIn[p].n += n;
  }
  function updateTransferOnInsertReclaimToken_r3(address t,address s,uint n) private    {
      updateTotalInOnInsertTransfer_r12(s,n);
      updateTotalOutOnInsertTransfer_r25(t,n);
      emit Transfer(t,s,n);
  }
  function updateBurnOnInsertRecv_burn_r17(uint n) private   returns (bool) {
      address p = msg.sender;
      uint m = balanceOf(p);
      if(n<=m) {
        updateTotalBurnOnInsertBurn_r20(p,n);
        updateAllBurnOnInsertBurn_r31(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r35() private    {
      totalBalances = TotalBalancesTuple(0,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        uint k = allowance[o][s].n;
        uint m = balanceOf(o);
        if(m>=n && k>=n) {
          updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
          updateTransferOnInsertTransferFrom_r0(o,r,n);
          return true;
        }
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r28(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r34(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r28(o,s,delta0);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r23(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(m>=n) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r9(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r8(address p,int m) private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int n) private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r19(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateAllowanceTotalOnInsertIncreaseAllowance_r34(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r2(address r,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        uint m = balanceOf(s);
        if(n<=m) {
          updateTotalOutOnInsertTransfer_r25(s,n);
          updateTotalInOnInsertTransfer_r12(r,n);
          emit Transfer(s,r,n);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r28(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updatePauseOnInsertRecv_pause_r10() private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          updatePausedOnInsertPause_r14(bool(true));
          emit Pause(true);
          return true;
        }
      }
      return false;
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateSendOnInsertReclaimToken_r24(address s,uint n) private    {
      payable(s).send(n);
  }
  function updatePendingOwnerOnInsertClaimOwnership_r13() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateTotalBurnOnInsertBurn_r20(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r8(p,delta0);
      totalBurn[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r33(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r34(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      // Empty()
  }
  function updateAllowanceOnIncrementSpentTotal_r28(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r9(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r28(o,s,delta0);
  }
  function updatePendingOwnerOnInsertTransferOwnership_r6(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r15() private   returns (bool) {
      address s = pendingOwner.p;
      if(s==msg.sender) {
        updatePendingOwnerOnInsertClaimOwnership_r13();
        updateOwnerOnInsertClaimOwnership_r32(s);
        emit ClaimOwnership(s);
        return true;
      }
      return false;
  }
  function updateUnpauseOnInsertRecv_unpause_r7() private   returns (bool) {
      if(true==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          emit Unpause(false);
          return true;
        }
      }
      return false;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r29(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updatePendingOwnerOnInsertTransferOwnership_r6(p);
          emit TransferOwnership(p);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      // Empty()
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r28(address o,address s,int d) private    {
      int _delta = int(-d);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateOwnerOnInsertClaimOwnership_r32(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateAllMintOnInsertMint_r18(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
      allMint.n += n;
  }
  function updateTotalMintOnInsertMint_r21(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
      totalMint[p].n += n;
  }
  function balanceOf(address p) private view  returns (uint) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateMintOnInsertRecv_mint_r11(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateTotalMintOnInsertMint_r21(p,n);
          updateAllMintOnInsertMint_r18(n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      // Empty()
  }
  function updatePausedOnInsertPause_r14(bool b) private    {
      paused = PausedTuple(b,true);
  }
  function updateReclaimTokenOnInsertRecv_reclaimToken_r5() private   returns (bool) {
      address s = msg.sender;
      if(s==owner.p) {
        address t = address(this);
        uint n = balanceOf(t);
        updateSendOnInsertReclaimToken_r24(s,n);
        updateTransferOnInsertReclaimToken_r3(t,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r31(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
      allBurn.n += n;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r12(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransfer_r25(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
      totalOut[p].n += n;
  }
}