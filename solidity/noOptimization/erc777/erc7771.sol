contract Erc777 {
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct RevokedDefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct DefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  struct OperatorsTuple {
    bool b;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
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
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  mapping(address=>DefaultOperatorTuple) defaultOperator;
  mapping(address=>TotalMintTuple) totalMint;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  mapping(address=>mapping(address=>RevokedDefaultOperatorTuple)) revokedDefaultOperator;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>mapping(address=>OperatorsTuple)) operators;
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  event Mint(address p,uint amount,uint data,uint odata);
  event Transfer(address o,address r,uint n,uint data,uint rdata,bool b);
  event IncreaseAllowance(address p,address s,uint n);
  event Burn(address p,uint amount,uint data,uint odata);
  constructor(uint name,uint symbol) public {
    updateTotalSupplyOnInsertConstructor_r6();
    updateOwnerOnInsertConstructor_r20();
  }
  function burn(uint amount,uint data) public    {
      bool r12 = updateBurnOnInsertRecv_burn_r12(amount,data);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function authorizeOperator(address o) public    {
      bool r14 = updateRevokedDefaultOperatorOnInsertRecv_authorizeOperator_r14(o);
      bool r23 = updateOperatorsOnInsertRecv_authorizeOperator_r23(o);
      if(r14==false && r23==false) {
        revert("Rule condition failed");
      }
  }
  function revokeOperator(address o) public    {
      bool r29 = updateRevokedDefaultOperatorOnInsertRecv_revokeOperator_r29(o);
      bool r28 = updateOperatorsOnInsertRecv_revokeOperator_r28(o);
      if(r29==false && r28==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function operatorBurn(address p,uint n,uint data,uint operatorData) public   returns (uint) {
      bool r17 = updateOperatorBurnOnInsertRecv_operatorBurn_r17(p,n,data,operatorData);
      bool r7 = updateOperatorBurnOnInsertRecv_operatorBurn_r7(p,n,data,operatorData);
      if(r17==false && r7==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function approve(address s,uint n) public    {
      bool r27 = updateIncreaseAllowanceOnInsertRecv_approve_r27(s,n);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getDefaultOperator(address p) public view  returns (bool) {
      bool b = defaultOperator[p].b;
      return b;
  }
  function transfer(address r,uint amount) public    {
      bool r2 = updateTransferOnInsertRecv_transfer_r2(r,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getOperators(address p,address o) public view  returns (bool) {
      bool b = operators[p][o].b;
      return b;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r25 = updateTransferFromOnInsertRecv_transferFrom_r25(from,to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function mint(uint amount,uint data) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(amount,data);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getRevokedDefaultOperator(address p,address o) public view  returns (bool) {
      bool b = revokedDefaultOperator[p][o].b;
      return b;
  }
  function operatorSend(address o,address r,uint n,uint data,uint operatorData) public   returns (uint) {
      bool r26 = updateOperatorSendOnInsertRecv_operatorSend_r26(o,r,n,data,operatorData);
      bool r5 = updateOperatorSendOnInsertRecv_operatorSend_r5(o,r,n,data,operatorData);
      bool r24 = updateOperatorSendOnInsertRecv_operatorSend_r24(o,r,n,data,operatorData);
      if(r26==false && r5==false && r24==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function updateTransferOnInsertRecv_transfer_r2(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m) {
        updateTotalOutOnInsertTransfer_r3(s,n);
        updateTotalInOnInsertTransfer_r0(r,n);
        emit Transfer(s,r,n,0,0,false);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r13(address p,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalBurn[p].n,_delta);
      updateBalanceOfOnInsertTotalBurn_r13(p,newValue);
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r26(address o,address r,uint n,uint data,uint operatorData) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[o].n;
      if(false==revokedDefaultOperator[o][s].b) {
        if(true==defaultOperator[s].b) {
          if(m>=n && r!=address(0) && n>0) {
            updateTransferOnInsertOperatorSend_r11(o,r,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r6() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r16(uint a) private    {
      int delta0 = int(a);
      updateTotalSupplyOnIncrementAllMint_r8(delta0);
      allMint.n += a;
  }
  function updateRevokedDefaultOperatorOnInsertRecv_authorizeOperator_r14(address o) private   returns (bool) {
      address p = msg.sender;
      if(true==defaultOperator[o].b) {
        if(p!=o) {
          revokedDefaultOperator[o][p] = RevokedDefaultOperatorTuple(false,true);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnDeleteAllowanceTotal_r15(address o,address s,uint m) private    {
      uint l = spentTotal[o][s].m;
      uint n = m-l;
      if(n==allowance[o][s].n) {
        allowance[o][s] = AllowanceTuple(0,false);
      }
  }
  function updateBalanceOfOnDeleteTotalOut_r13(address p,uint o) private    {
      uint i = totalIn[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      if(s==balanceOf[p].n) {
        balanceOf[p] = BalanceOfTuple(0,false);
      }
  }
  function updateOperatorsOnInsertRecv_authorizeOperator_r23(address o) private   returns (bool) {
      address p = msg.sender;
      if(false==defaultOperator[o].b) {
        if(p!=o) {
          operators[p][o] = OperatorsTuple(true,true);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r8(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allMint.n,_delta);
      updateTotalSupplyOnInsertAllMint_r8(newValue);
  }
  function updateAllowanceOnInsertSpentTotal_r15(address o,address s,uint l) private    {
      SpentTotalTuple memory toDelete = spentTotal[o][s];
      if(toDelete._valid==true) {
        updateAllowanceOnDeleteSpentTotal_r15(o,s,toDelete.m);
      }
      uint m = allowanceTotal[o][s].m;
      uint n = m-l;
      allowance[o][s] = AllowanceTuple(n,true);
  }
  function updateOwnerOnInsertConstructor_r20() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnDeleteTotalIn_r13(address p,uint i) private    {
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      if(s==balanceOf[p].n) {
        balanceOf[p] = BalanceOfTuple(0,false);
      }
  }
  function updateBalanceOfOnIncrementTotalOut_r13(address p,int o) private    {
      int _delta = int(o);
      uint newValue = updateuintByint(totalOut[p].n,_delta);
      updateBalanceOfOnInsertTotalOut_r13(p,newValue);
  }
  function updateTotalBurnOnInsertBurn_r4(address p,uint a) private    {
      int delta0 = int(a);
      updateBalanceOfOnIncrementTotalBurn_r13(p,delta0);
      totalBurn[p].n += a;
  }
  function updateBalanceOfOnInsertTotalMint_r13(address p,uint n) private    {
      TotalMintTuple memory toDelete = totalMint[p];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteTotalMint_r13(p,toDelete.n);
      }
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint s = ((n+i)-m)-o;
      balanceOf[p] = BalanceOfTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r3(address p,uint n) private    {
      int delta2 = int(n);
      updateBalanceOfOnIncrementTotalOut_r13(p,delta2);
      totalOut[p].n += n;
  }
  function updateTotalInOnInsertTransfer_r0(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r13(p,delta0);
      totalIn[p].n += n;
  }
  function updateTotalSupplyOnInsertAllBurn_r8(uint b) private    {
      uint m = allMint.n;
      uint n = m-b;
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r15(address o,address s,int l) private    {
      int _delta = int(l);
      uint newValue = updateuintByint(spentTotal[o][s].m,_delta);
      updateAllowanceOnInsertSpentTotal_r15(o,s,newValue);
  }
  function updateBurnOnInsertOperatorBurn_r21(address p,uint n,uint d,uint o) private    {
      updateAllBurnOnInsertBurn_r1(n);
      updateTotalBurnOnInsertBurn_r4(p,n);
      emit Burn(p,n,d,o);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r15(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowanceTotal[o][s].m,_delta);
      updateAllowanceOnInsertAllowanceTotal_r15(o,s,newValue);
  }
  function updateAllowanceOnDeleteSpentTotal_r15(address o,address s,uint l) private    {
      uint m = allowanceTotal[o][s].m;
      uint n = m-l;
      if(n==allowance[o][s].n) {
        allowance[o][s] = AllowanceTuple(0,false);
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,uint n) private    {
      int delta1 = int(n);
      updateAllowanceOnIncrementSpentTotal_r15(o,s,delta1);
      spentTotal[o][s].m += n;
  }
  function updateBalanceOfOnInsertTotalBurn_r13(address p,uint m) private    {
      TotalBurnTuple memory toDelete = totalBurn[p];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteTotalBurn_r13(p,toDelete.n);
      }
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      balanceOf[p] = BalanceOfTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r9(uint a,uint d) private   returns (bool) {
      address s = msg.sender;
      updateTotalMintOnInsertMint_r19(s,a);
      updateAllMintOnInsertMint_r16(a);
      emit Mint(s,a,d,0);
      return true;
      return false;
  }
  function updateAllowanceOnInsertAllowanceTotal_r15(address o,address s,uint m) private    {
      AllowanceTotalTuple memory toDelete = allowanceTotal[o][s];
      if(toDelete._valid==true) {
        updateAllowanceOnDeleteAllowanceTotal_r15(o,s,toDelete.m);
      }
      uint l = spentTotal[o][s].m;
      uint n = m-l;
      allowance[o][s] = AllowanceTuple(n,true);
  }
  function updateOperatorsOnInsertRecv_revokeOperator_r28(address o) private   returns (bool) {
      address p = msg.sender;
      if(false==defaultOperator[o].b) {
        if(p!=o) {
          operators[p][o] = OperatorsTuple(false,true);
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
  function updateIncreaseAllowanceOnInsertRecv_approve_r27(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r30(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertAllMint_r8(uint m) private    {
      uint b = allBurn.n;
      uint n = m-b;
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTotalMintOnInsertMint_r19(address p,uint a) private    {
      int delta1 = int(a);
      updateBalanceOfOnIncrementTotalMint_r13(p,delta1);
      totalMint[p].n += a;
  }
  function updateBalanceOfOnDeleteTotalMint_r13(address p,uint n) private    {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint s = ((n+i)-m)-o;
      if(s==balanceOf[p].n) {
        balanceOf[p] = BalanceOfTuple(0,false);
      }
  }
  function updateTransferOnInsertTransferFrom_r18(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r0(r,n);
      updateTotalOutOnInsertTransfer_r3(o,n);
      emit Transfer(o,r,n,0,0,false);
  }
  function updateTransferOnInsertOperatorSend_r11(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r0(r,n);
      updateTotalOutOnInsertTransfer_r3(o,n);
      emit Transfer(o,r,n,0,0,false);
  }
  function updateBurnOnInsertRecv_burn_r12(uint a,uint d) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(a<=m) {
        updateTotalBurnOnInsertBurn_r4(s,a);
        updateAllBurnOnInsertBurn_r1(a);
        emit Burn(s,a,d,0);
        return true;
      }
      return false;
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r5(address o,address r,uint n,uint data,uint operatorData) private   returns (bool) {
      if(o==msg.sender) {
        uint m = balanceOf[o].n;
        if(m>=n && r!=address(0) && n>0) {
          updateTransferOnInsertOperatorSend_r11(o,r,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r8(int b) private    {
      int _delta = int(b);
      uint newValue = updateuintByint(allBurn.n,_delta);
      updateTotalSupplyOnInsertAllBurn_r8(newValue);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r30(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r15(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateRevokedDefaultOperatorOnInsertRecv_revokeOperator_r29(address o) private   returns (bool) {
      address p = msg.sender;
      if(true==defaultOperator[o].b) {
        if(p!=o) {
          revokedDefaultOperator[p][o] = RevokedDefaultOperatorTuple(true,true);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnInsertTotalOut_r13(address p,uint o) private    {
      TotalOutTuple memory toDelete = totalOut[p];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteTotalOut_r13(p,toDelete.n);
      }
      uint i = totalIn[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      balanceOf[p] = BalanceOfTuple(s,true);
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r24(address o,address r,uint n,uint data,uint operatorData) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[o].n;
      if(true==operators[o][s].b) {
        if(m>=n && r!=address(0) && n>0) {
          updateTransferOnInsertOperatorSend_r11(o,r,n);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnInsertTotalIn_r13(address p,uint i) private    {
      TotalInTuple memory toDelete = totalIn[p];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteTotalIn_r13(p,toDelete.n);
      }
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      balanceOf[p] = BalanceOfTuple(s,true);
  }
  function updateAllBurnOnInsertBurn_r1(uint a) private    {
      int delta0 = int(a);
      updateTotalSupplyOnIncrementAllBurn_r8(delta0);
      allBurn.n += a;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf[o].n;
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r18(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnDeleteTotalBurn_r13(address p,uint m) private    {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      if(s==balanceOf[p].n) {
        balanceOf[p] = BalanceOfTuple(0,false);
      }
  }
  function updateOperatorBurnOnInsertRecv_operatorBurn_r7(address p,uint n,uint data,uint operatorData) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[p].n;
      if(true==operators[p][s].b) {
        if(m>=n && p!=address(0) && n>0) {
          updateBurnOnInsertOperatorBurn_r21(p,n,data,operatorData);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r13(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(totalMint[p].n,_delta);
      updateBalanceOfOnInsertTotalMint_r13(p,newValue);
  }
  function updateBalanceOfOnIncrementTotalIn_r13(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(totalIn[p].n,_delta);
      updateBalanceOfOnInsertTotalIn_r13(p,newValue);
  }
  function updateOperatorBurnOnInsertRecv_operatorBurn_r17(address p,uint n,uint data,uint operatorData) private   returns (bool) {
      address s = msg.sender;
      if(false==revokedDefaultOperator[p][s].b) {
        uint m = balanceOf[p].n;
        if(true==defaultOperator[s].b) {
          if(m>=n && p!=address(0) && n>0) {
            updateBurnOnInsertOperatorBurn_r21(p,n,data,operatorData);
            return true;
          }
        }
      }
      return false;
  }
}