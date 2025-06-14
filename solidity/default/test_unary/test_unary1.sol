/// math.sol -- mixin for inline numerical wizardry

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

pragma solidity >0.4.13;

contract DSMath {
    function add(uint x, uint y) internal pure returns (uint z) {
        require((z = x + y) >= x, "ds-math-add-overflow");
    }
    function sub(uint x, uint y) internal pure returns (uint z) {
        require((z = x - y) <= x, "ds-math-sub-underflow");
    }
    function mul(uint x, uint y) internal pure returns (uint z) {
        require(y == 0 || (z = x * y) / y == x, "ds-math-mul-overflow");
    }

    function min(uint x, uint y) internal pure returns (uint z) {
        return x <= y ? x : y;
    }
    function max(uint x, uint y) internal pure returns (uint z) {
        return x >= y ? x : y;
    }
    function imin(int x, int y) internal pure returns (int z) {
        return x <= y ? x : y;
    }
    function imax(int x, int y) internal pure returns (int z) {
        return x >= y ? x : y;
    }

    uint constant WAD = 10 ** 18;
    uint constant RAY = 1; // modified to be used for int

    //rounds to zero if x*y < WAD / 2
    function wmul(uint x, uint y) internal pure returns (uint z) {
        z = add(mul(x, y), WAD / 2) / WAD;
    }
    //rounds to zero if x*y < WAD / 2
    function rmul(uint x, uint y) internal pure returns (uint z) {
        z = add(mul(x, y), RAY / 2) / RAY;
    }
    //rounds to zero if x*y < WAD / 2
    function wdiv(uint x, uint y) internal pure returns (uint z) {
        z = add(mul(x, WAD), y / 2) / y;
    }
    //rounds to zero if x*y < RAY / 2
    function rdiv(uint x, uint y) internal pure returns (uint z) {
        z = add(mul(x, RAY), y / 2) / y;
    }

    // This famous algorithm is called "exponentiation by squaring"
    // and calculates x^n with x as fixed-point and n as regular unsigned.
    //
    // It's O(log n), instead of O(n) for naive repeated multiplication.
    //
    // These facts are why it works:
    //
    //  If n is even, then x^n = (x^2)^(n/2).
    //  If n is odd,  then x^n = x * x^(n-1),
    //   and applying the equation for even x gives
    //    x^n = x * (x^2)^((n-1) / 2).
    //
    //  Also, EVM division is flooring and
    //    floor[(n-1) / 2] = floor[n / 2].
    //
    function rpow(uint x, uint n) internal pure returns (uint z) {
        z = n % 2 != 0 ? x : RAY;

        for (n /= 2; n != 0; n /= 2) {
            x = rmul(x, x);

            if (n % 2 != 0) {
                z = rmul(z, x);
            }
        }
    }
}
import "@openzeppelin/contracts/utils/math/Math.sol";
contract Test_unary {
  struct GetApprovedTuple {
    address approved;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct OwnerOfTuple {
    address o;
    bool _valid;
  }
  struct IsApprovedForAllTuple {
    uint approved;
    bool _valid;
  }
  mapping(uint=>GetApprovedTuple) getApproved;
  mapping(uint=>OwnerOfTuple) ownerOf;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  OwnerTuple owner;
  event TransferFrom(address from,address to,uint tokenId);
  event SetApprovalForAll(address o,address operator,uint approved);
  event Approve(address o,address approved,uint tokenId);
  event Burn(uint tokenId);
  event Transfer(address from,address to,uint tokenId);
  event Mint(address to,uint tokenId);
  constructor() public {
    updateOwnerOnInsertConstructor_r9();
  }
  function burn(uint tokenId) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(tokenId);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint tokenId) public    {
      bool r3 = updateTransferOnInsertRecv_transfer_r3(to,tokenId);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address to,uint tokenId) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(to,tokenId);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address o = ownerOf[tokenId].o;
      return o;
  }
  function transferFrom(address from,address to,uint tokenId) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(from,to,tokenId);
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(from,to,tokenId);
      bool r6 = updateTransferFromOnInsertRecv_transferFrom_r6(from,to,tokenId);
      if(r8==false && r2==false && r6==false) {
        revert("Rule condition failed");
      }
  }
  function setApprovalForAll(address operator,uint approved) public    {
      bool r17 = updateSetApprovalForAllOnInsertRecv_setApprovalForAll_r17(operator,approved);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getGetApproved(uint tokenId) public view  returns (address) {
      address approved = getApproved[tokenId].approved;
      return approved;
  }
  function approve(address approved,uint tokenId) public    {
      bool r14 = updateApproveOnInsertRecv_approve_r14(approved,tokenId);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getIsApprovedForAll(address o,address operator) public view  returns (uint) {
      uint approved = isApprovedForAll[o][operator].approved;
      return approved;
  }
  function updateTransferOnInsertRecv_transfer_r3(address to,uint tokenId) private   returns (bool) {
      address from = msg.sender;
      if(from==ownerOf[tokenId].o) {
        if(to!=address(0)) {
          updateGetApprovedOnInsertTransfer_r12(tokenId);
          updateOwnerOfOnInsertTransfer_r5(to,tokenId);
          emit Transfer(from,to,tokenId);
          return true;
        }
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address from,address to,uint tokenId) private   returns (bool) {
      address spender = msg.sender;
      uint approved = isApprovedForAll[from][spender].approved;
      if(from==ownerOf[tokenId].o) {
        if(to!=address(0) && approved==1) {
          updateGetApprovedOnInsertTransferFrom_r10(tokenId);
          updateOwnerOfOnInsertTransferFrom_r1(to,tokenId);
          emit TransferFrom(from,to,tokenId);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOfOnInsertTransfer_r5(address to,uint tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateGetApprovedOnInsertBurn_r15(uint tokenId) private    {
      getApproved[tokenId] = GetApprovedTuple(address(0),true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r6(address from,address to,uint tokenId) private   returns (bool) {
      address spender = msg.sender;
      if(spender==getApproved[tokenId].approved) {
        if(from==ownerOf[tokenId].o) {
          if(Math.sqrt(DSMath.rpow(tokenId,100))!=Math.sqrt(tokenId+2) && to!=address(0)) {
            uint y = Math.sqrt(DSMath.rpow(tokenId,100));
            updateGetApprovedOnInsertTransferFrom_r10(tokenId);
            updateOwnerOfOnInsertTransferFrom_r1(to,tokenId);
            emit TransferFrom(from,to,tokenId);
            return true;
          }
        }
      }
      return false;
  }
  function updateOwnerOfOnInsertMint_r11(address to,uint tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateOwnerOfOnInsertBurn_r0(uint tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(address(0),true);
  }
  function updateGetApprovedOnInsertApprove_r7(address approved,uint tokenId) private    {
      getApproved[tokenId] = GetApprovedTuple(approved,true);
  }
  function updateBurnOnInsertRecv_burn_r4(uint tokenId) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        address currentOwner = ownerOf[tokenId].o;
        if(currentOwner!=address(0)) {
          updateOwnerOfOnInsertBurn_r0(tokenId);
          updateGetApprovedOnInsertBurn_r15(tokenId);
          emit Burn(tokenId);
          return true;
        }
      }
      return false;
  }
  function updateApproveOnInsertRecv_approve_r14(address approved,uint tokenId) private   returns (bool) {
      address o = msg.sender;
      if(o==ownerOf[tokenId].o) {
        updateGetApprovedOnInsertApprove_r7(approved,tokenId);
        emit Approve(o,approved,tokenId);
        return true;
      }
      return false;
  }
  function updateGetApprovedOnInsertTransfer_r12(uint tokenId) private    {
      getApproved[tokenId] = GetApprovedTuple(address(0),true);
  }
  function updateSetApprovalForAllOnInsertRecv_setApprovalForAll_r17(address operator,uint approved) private   returns (bool) {
      address o = msg.sender;
      if(operator!=address(0)) {
        updateIsApprovedForAllOnInsertSetApprovalForAll_r18(o,operator,approved);
        emit SetApprovalForAll(o,operator,approved);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address from,address to,uint tokenId) private   returns (bool) {
      address spender = msg.sender;
      if(spender==ownerOf[tokenId].o) {
        if(to!=address(0) && spender==from) {
          updateGetApprovedOnInsertTransferFrom_r10(tokenId);
          updateOwnerOfOnInsertTransferFrom_r1(to,tokenId);
          emit TransferFrom(from,to,tokenId);
          return true;
        }
      }
      return false;
  }
  function updateIsApprovedForAllOnInsertSetApprovalForAll_r18(address o,address operator,uint approved) private    {
      isApprovedForAll[o][operator] = IsApprovedForAllTuple(approved,true);
  }
  function updateGetApprovedOnInsertTransferFrom_r10(uint tokenId) private    {
      getApproved[tokenId] = GetApprovedTuple(address(0),true);
  }
  function updateOwnerOfOnInsertTransferFrom_r1(address to,uint tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateMintOnInsertRecv_mint_r13(address to,uint tokenId) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(address(0)==ownerOf[tokenId].o) {
          if(to!=address(0)) {
            updateOwnerOfOnInsertMint_r11(to,tokenId);
            emit Mint(to,tokenId);
            return true;
          }
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
}