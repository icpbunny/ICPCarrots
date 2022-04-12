/*
Basic single token per canister
*/

import Cycles "mo:base/ExperimentalCycles";
import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Bool "mo:base/Bool";
import Nat64 "mo:base/Nat64";
import Blob "mo:base/Blob";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Char "mo:base/Char";

import Int "mo:base/Int";
import Time "mo:base/Time";
import Text "mo:base/Text";
import Float "mo:base/Float";


//Get the path right
import AID "../motoko/util/AccountIdentifier";
import ExtCore "../motoko/ext/Core";
import ExtCommon "../motoko/ext/Common";
import Debug "mo:base/Debug";

actor class standard_token(init_name: Text, init_symbol: Text, init_decimals: Nat8, init_supply: ExtCore.Balance, init_owner: Principal) = this{
  
  // Types
  type AccountIdentifier = ExtCore.AccountIdentifier;
  type SubAccount = ExtCore.SubAccount;
  type User = ExtCore.User;
  type Balance = ExtCore.Balance;
  type TokenIdentifier = ExtCore.TokenIdentifier;
  type Extension = ExtCore.Extension;
  type CommonError = ExtCore.CommonError;
  type NotifyService = ExtCore.NotifyService;
  
  type BalanceRequest = ExtCore.BalanceRequest;
  type BalanceResponse = ExtCore.BalanceResponse;
  type TransferRequest = ExtCore.TransferRequest;
  type TransferResponse = ExtCore.TransferResponse;
  type Metadata = ExtCommon.Metadata;

  type TokenCarrots = {
    carrot_count : Nat;
    owner : Principal;
  };  
  
  private stable let totalBunny_ : Nat = 10000;             
  private stable var carrotsCirculation_ : Nat = 0;             

  func isEqP(x: Principal, y: Principal): Bool { x == y };
  func isEq(x: Nat, y: Nat): Bool { x == y };

  private var lastClaimed_ = HashMap.HashMap<Nat, Nat>(totalBunny_, isEq,  Nat32.fromNat);
  stable var _lastClaimed : [(Nat, Nat)] = []; 
  stable var claimStart : Nat = 1636502400;  //1636410401
  stable var _claimStart : Text = "November 10, 2021 12:00:00 AM";

  private let EXTENSIONS : [Extension] = ["@ext/common"];
  
  //State work
  private stable var _balancesState : [(AccountIdentifier, Balance)] = [(AID.fromPrincipal(init_owner, null), init_supply)];
  private var _balances : HashMap.HashMap<AccountIdentifier, Balance> = HashMap.fromIter(_balancesState.vals(), 0, AID.equal, AID.hash);
  private stable let METADATA : Metadata = #fungible({
    name = init_name;
    symbol = init_symbol;
    decimals = init_decimals;
    metadata = null;
  }); 

  var debugMessage :Text = "";
  var _isTransferEnabled : Bool = false;

  let BunnyCanister  = actor "xkbqi-2qaaa-aaaah-qbpqq-cai" : actor { 
      carrots_of : shared query Nat -> async TokenCarrots;
      user_tokens : shared query Principal -> async [Nat];
 };

  private stable var _supply : Balance  = init_supply;
  let limit = 50_000_000_000_000;

  //State functions
  system func preupgrade() {
     _lastClaimed := Iter.toArray(lastClaimed_.entries());
    _balancesState := Iter.toArray(_balances.entries());
  };
  system func postupgrade() {

     lastClaimed_ := HashMap.fromIter<Nat, Nat>(_lastClaimed.vals(),_lastClaimed.size(), isEq,  Nat32.fromNat);
     _balances := HashMap.fromIter(_balancesState.vals(), _balancesState.size(), AID.equal, AID.hash);

    _balancesState := [];
    _lastClaimed := [];
  };

  //(AccountIdentifier, Balance)  BalanceResponse

  public shared(msg) func maxsupply() : async Nat {

    if(msg.caller == init_owner)
    {
      let aid = AID.fromPrincipal(init_owner, null);
      let result = _balances.put(aid, 100000000000000000000);
      carrotsCirculation_ := 0;
      return 1000000000000;
    }
    else
    {
      let aid = AID.fromPrincipal(init_owner, null);
      switch (_balances.get(aid)) {
        case (?balance) {
          return Nat.div(balance,100000000);
        };
        case (_) {
          return 0;
        };
      };
    };
        
  };   

  public query func wallet_balance () : async Nat 
  {
      let aid = AID.fromPrincipal(init_owner, null);
      switch (_balances.get(aid)) {
        case (?balance) {
          return Nat.div(balance,100000000);
        };
        case (_) {
          return 0;
        };
      };
  };

  public query func holders() : async Nat  {
      _balances.size();
   };

  public shared(msg) func getOwners() : async [(AccountIdentifier, Balance)]  {

      if(msg.caller == init_owner)
      {
        _balancesState := Iter.toArray(_balances.entries());
        return _balancesState;
      };
      []; 
   };

   private func textToNat( txt : Text) : Nat {
        assert(txt.size() > 0);
        let chars = txt.chars();

        var num : Nat = 0;
        for (v in chars){
            let charToNum = Nat32.toNat(Char.toNat32(v)-48);
            assert(charToNum >= 0 and charToNum <= 9);
            num := num * 10 +  charToNum;          
        };
        num;
    };

  private func substring( txt : Nat,leng: Nat) : Text {
       
      assert(Nat.toText(txt).size() > 0);

      let chars = Nat.toText(txt).chars();

      var num : Nat = 0;
      var substr : Text = "";
      for (v in chars)
      {
          if(Nat.less(num,leng))
          {
              substr := substr # Char.toText(v); 
              num := num+1;
          };
        };
        substr;
    }; 

  public shared(msg) func claimFor(user: Principal) : async Nat {

    var userTokens : [Nat] =  await BunnyCanister.user_tokens(user);
    var tokenID : Nat =0;
    var carrotsHarvested : Nat = 0;
    var _carrots : Nat = 0;

    debugMessage := debugMessage # " Principal ="# Principal.toText(user);

    for (tokenId in userTokens.vals()) {

        debugMessage := debugMessage # " Token ="# Nat.toText(tokenId);

      _carrots  := await claimTokenCarrots(tokenId);
      carrotsHarvested := Nat.add(carrotsHarvested,_carrots);

      debugMessage := debugMessage # " carrotsHarvested ="# Nat.toText(carrotsHarvested);

     };     
    return carrotsHarvested;
  };

  // Burn Carrots of particular user
  public shared(msg) func burnCarrots() : async Nat {
    
    if(msg.caller == init_owner)
      {

        _balancesState := []; 
        _balances := HashMap.fromIter(_balancesState.vals(), _balancesState.size(), AID.equal, AID.hash);
        return 1;
      };
      return 0;
  };

  public shared(msg) func claimCarrots() : async Nat {

    /*
      1.Check whether burn for particular principal
      2.If Burn then do nothing
      3.Otherwise add to Burn HashMap and set Balance to Zero
      4.Check whether current time is great than Nov 20 00:00 2021
    */

    var userTokens : [Nat] =  await BunnyCanister.user_tokens(msg.caller);
    var tokenID : Nat =0;
    var carrotsHarvested : Nat = 0;
    var _carrots : Nat = 0;

    for (tokenId in userTokens.vals()) {
      _carrots  := await claimTokenCarrots(tokenId);
      carrotsHarvested := Nat.add(carrotsHarvested,_carrots);
     };     
     carrotsCirculation_ := Nat.add(carrotsCirculation_, carrotsHarvested);
    return carrotsHarvested;
  };

  public shared(msg) func claimTokenCarrots(tokenID: Nat) : async Nat {

    var tokenCarrots : TokenCarrots = {
      carrot_count = 0;
      owner = init_owner;
    };  
    
    var blobArray : [Nat8] = [0,1,2,3];
    var carrotsHarvested : Nat = 0;

    tokenCarrots := await BunnyCanister.carrots_of(tokenID);

    debugMessage := debugMessage # "Token ID Carrots " # Nat.toText(tokenID) # " Carrots " # Nat.toText(tokenCarrots.carrot_count);

    /*
    1.Get the lastclaimed value
    2.Get the current time
    3.Calculate the difference between current time and lastClaimed
    4.Calculate the diffence in hours
    5.Divide number of carrot per hour  If per day carrots = 12 the 0.5 carrots per hour
    6.Multiple number of hours * number of carrots per hour
    Sat Nov 20 2021 06:00:00 GMT+0000
    */

    var _current_Time : Nat = textToNat(Int.toText(Time.now()));
    let lastClaimed : ?Nat = lastClaimed_.get(tokenID);
    var _lastClaimTime : Nat = 0;
    var _unClaimedTime : Nat = 0;

    switch (lastClaimed) 
    {
      case (?lastClaimed) {
          _lastClaimTime := lastClaimed;
      };
      case (_) {
          _lastClaimTime := claimStart; //Never claimed, Nov 20th 
      }
    };


    //_lastClaimTime := claimStart; // Always allow claim

    debugMessage := debugMessage # "Last Claim " # Nat.toText(_lastClaimTime);


    //_unClaimedTime := Int.sub(_current_Time , _lastClaimTime);
    let t1      : Text = substring(_current_Time ,10);
    let t2      : Text = substring(_lastClaimTime,10);


    _current_Time  := textToNat(t1);
    _lastClaimTime  := textToNat(t2);

    debugMessage := debugMessage # "_current_Time " # Nat.toText(_current_Time);
    debugMessage := debugMessage # "_lastClaimTime " # Nat.toText(_lastClaimTime);

    /*if(Nat.less(_current_Time , _lastClaimTime))
    {
      return 100000000; // Still Claim not startd yet
    };*/

    let unClaimedTime :Nat = Nat.sub(_current_Time , _lastClaimTime);

    debugMessage := debugMessage # "Unclaimed Time " # Nat.toText(unClaimedTime);


    let hours = Nat.div(unClaimedTime,3600);

    debugMessage:= debugMessage# "Hours =" # Nat.toText(hours);

    let carrots_  = tokenCarrots.carrot_count;
    let hours_    = 24;

    let carrotPerHour :  Float = Float.div(Float.fromInt(carrots_),Float.fromInt(hours_));


    var carrots = Float.mul(Float.fromInt(hours),carrotPerHour);

    carrots := Float.mul(carrots,100000000); // 8 Zero

    let carrotsCount_ : Int = Float.toInt(carrots);

    var _carrots_ : Nat =  Int.abs(carrotsCount_); 

    debugMessage:= debugMessage# "_carrots_ =" # Nat.toText(_carrots_);

    
    var claimRequest : TransferRequest = 
    {
      from = #principal(init_owner);
      to = #principal(tokenCarrots.owner);
      token = "ICPCarrots";
      amount = _carrots_;
      memo = Blob.fromArray(blobArray);
      notify = false;
      subaccount = null;
    };

    var response = await claim(claimRequest);
    debugMessage:= debugMessage# "response =" #response;

    if(Text.equal(response,"Transfer Successful!"))
    {
      var _claimedTime : Nat = textToNat(Int.toText(Time.now()));

      lastClaimed_.put(tokenID, _claimedTime);

      carrotsHarvested := Nat.add(carrotsHarvested,_carrots_)
    };
   
    return carrotsHarvested;
  };
  
  /*******************************************************************/ 
    public shared(msg) func walletTransfer(amount:Nat,to:Text) : async Text {
  
    var blobArray : [Nat8] = [0,1,32,4];

    var claimRequest : TransferRequest = {
    from = #principal(Principal.fromText(Principal.toText(msg.caller)));
    to = #principal(Principal.fromText(to));
    token = "";
    amount = amount;
    memo = Blob.fromArray(blobArray);
    notify = false;
    subaccount = null;
    };

    var response = await claim(claimRequest);
    Debug.print("Transfer testTransfer "#response);
    return Nat.toText(amount) # " Carrots transfered from your account : " # response;
  };
  /*******************************************************************/

  private func claim(request: TransferRequest) : async Text {

    let sender = ExtCore.User.toAID(request.from);
    let receiver = ExtCore.User.toAID(request.to);

   
    switch (_balances.get(sender)) {
      case (?sender_balance) {
        if (sender_balance >= request.amount) {
          //Remove from sender first
          var sender_balance_new : Balance = sender_balance - request.amount;
          _balances.put(sender, sender_balance_new);
          
          var provisional_amount : Balance = request.amount;
          if (request.notify) {
            switch(ExtCore.User.toPrincipal(request.to)) {
              case (?canisterId) {
                let notifier : NotifyService = actor(Principal.toText(canisterId));
                switch(await notifier.tokenTransferNotification(request.token, request.from, request.amount, request.memo)) {
                  case (?balance) {
                    provisional_amount := balance;
                  };
                  case (_) {
                    var sender_balance_new2 = switch (_balances.get(sender)) {
                      case (?sender_balance) {
                          sender_balance + request.amount;
                      };
                      case (_) {
                          request.amount;
                      };
                    };
                    _balances.put(sender, sender_balance_new2);
                    Debug.print("Transfer Rejected");
                    return "Transfer Rejected";
                  };
                };
              };
              case (_) {
                var sender_balance_new2 = switch (_balances.get(sender)) {
                  case (?sender_balance) {
                      sender_balance + request.amount;
                  };
                  case (_) {
                      request.amount;
                  };
                };
                _balances.put(sender, sender_balance_new2);
                return "CannotNotify";

              }
            };
          };
          assert(provisional_amount <= request.amount); //should never hit
          
          var receiver_balance_new = switch (_balances.get(receiver)) {
            case (?receiver_balance) {
                receiver_balance + provisional_amount;
            };
            case (_) {
                provisional_amount;
            };
          };
          _balances.put(receiver, receiver_balance_new);
          
          //Process sender refund
          if (provisional_amount < request.amount) {
            var sender_refund : Balance = request.amount - provisional_amount;
            var sender_balance_new2 = switch (_balances.get(sender)) {
              case (?sender_balance) {
                  sender_balance + sender_refund;
              };
              case (_) {
                  sender_refund;
              };
            };
            _balances.put(sender, sender_balance_new2);
          };
          
          return "Transfer Successful!";
        } else {
          Debug.print("Transfer InsufficientBalance");
          return "InsufficientBalance !";
          
        };
      };
      case (_) {
        Debug.print("Transfer InsufficientBalance");

        return "InsufficientBalance!" # sender;
      };
    };
  };

  public shared(msg) func carrotTransfer(request: TransferRequest) : async TransferResponse {

    debugMessage := debugMessage # " Transfer ";
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
       debugMessage := debugMessage # " " # "Transfer InvalidToken";
			return #err(#InvalidToken(request.token));
		};
    if (ExtCore.TokenIdentifier.getIndex(request.token) != 0) {
      debugMessage := debugMessage # " " # "Transfer InvalidToken";

			return #err(#InvalidToken(request.token));
		};
    
    let sender = ExtCore.User.toAID(request.from);
    let spender = AID.fromPrincipal(msg.caller, request.subaccount);
    let receiver = ExtCore.User.toAID(request.to);

    
    if (AID.equal(sender, spender) == false) {
        debugMessage := debugMessage # " " # "Transfer Unauthorized Spender = " # spender #" Sender=" # sender #  Principal.toText(msg.caller);
        return #err(#Unauthorized(spender));
    }; 

   
    switch (_balances.get(sender)) {
      case (?sender_balance) {
        if (sender_balance >= request.amount) {
          //Remove from sender first
          var sender_balance_new : Balance = sender_balance - request.amount;
          _balances.put(sender, sender_balance_new);
          
          var provisional_amount : Balance = request.amount;
          if (request.notify) {
            switch(ExtCore.User.toPrincipal(request.to)) {
              case (?canisterId) {
                let notifier : NotifyService = actor(Principal.toText(canisterId));
                switch(await notifier.tokenTransferNotification(request.token, request.from, request.amount, request.memo)) {
                  case (?balance) {
                    provisional_amount := balance;
                  };
                  case (_) {
                    var sender_balance_new2 = switch (_balances.get(sender)) {
                      case (?sender_balance) {
                          sender_balance + request.amount;
                      };
                      case (_) {
                          request.amount;
                      };
                    };
                    _balances.put(sender, sender_balance_new2);
                    Debug.print("Transfer Rejected");
                    return #err(#Rejected);
                  };
                };
              };
              case (_) {
                var sender_balance_new2 = switch (_balances.get(sender)) {
                  case (?sender_balance) {
                      sender_balance + request.amount;
                  };
                  case (_) {
                      request.amount;
                  };
                };
                _balances.put(sender, sender_balance_new2);
                return #err(#CannotNotify(receiver));
                Debug.print("Transfer CannotNotify");

              }
            };
          };
          assert(provisional_amount <= request.amount); //should never hit
          assert(provisional_amount <= request.amount); //should never hit

          debugMessage := debugMessage # " request.amount " # Nat.toText(request.amount) # "provisional_amount " # Nat.toText(provisional_amount);

          var receiver_balance_new = switch (_balances.get(receiver)) {
            case (?receiver_balance) {
                receiver_balance + provisional_amount;
            };
            case (_) {
                provisional_amount;
            };
          };
          _balances.put(receiver, receiver_balance_new);
          
          //Process sender refund
          if (provisional_amount < request.amount) {
            var sender_refund : Balance = request.amount - provisional_amount;
            var sender_balance_new2 = switch (_balances.get(sender)) {
              case (?sender_balance) {
                  sender_balance + sender_refund;
              };
              case (_) {
                  sender_refund;
              };
            };
            _balances.put(sender, sender_balance_new2);
          };
          
          return #ok(provisional_amount);
        } else {
          Debug.print("Transfer InsufficientBalance");
          return #err(#InsufficientBalance);
          
        };
      };
      case (_) {
        Debug.print("Transfer InsufficientBalance");

        return #err(#InsufficientBalance);
      };
    };
  };

  public shared(msg) func resetDebug(code:Nat) : async Text {
    assert(code == 5871);
    debugMessage := "";
    return "Debug cleared!";
  };    
  // Temporary
  public shared(msg) func resetClaim() : async Text  {
    
    if(msg.caller == init_owner)
    {

    _lastClaimed := [];

      lastClaimed_ := HashMap.fromIter<Nat, Nat>(_lastClaimed.vals(),_lastClaimed.size(), isEq,  Nat32.fromNat);
      claimStart := 1636502400;
      
      return "Last claim and Claim Start date set to November 10, 2021 12:00:00 AM GMT!";
    };
    return "OK";
  };    

  public shared(msg) func testClaim() : async Text  {
    
    if(msg.caller == init_owner)
    {

      _lastClaimed := [];
      lastClaimed_ := HashMap.fromIter<Nat, Nat>(_lastClaimed.vals(),_lastClaimed.size(), isEq,  Nat32.fromNat);
      claimStart := 1636502400;
      
      return "Last claim and Claim Start date set to November 8, 2021 10:26:41 PM GMT!";
    };
    return "OK!";
  };    

  public shared(msg) func claimStartDate() : async Text  {
    if(msg.caller == init_owner)
    {
        claimStart := 1636502400;
    };
    return _claimStart;
  };

  public shared(msg) func disableTransfer() : async Bool  {
    if(msg.caller == init_owner)
    {
        _isTransferEnabled := false;
    };
    _isTransferEnabled;
  };

  public shared(msg) func enableTransfer() : async Bool  {
    if(msg.caller == init_owner)
    {
        _isTransferEnabled := true;
    };
    _isTransferEnabled;
  };

  public shared(msg) func transfer(request: TransferRequest) : async TransferResponse {

    if(_isTransferEnabled == false)
    {
      return #err(#Unauthorized("We are in test phase.Transfer is disabled."));
    };

    debugMessage := debugMessage # " Transfer ";
    if (ExtCore.TokenIdentifier.isPrincipal(request.token, Principal.fromActor(this)) == false) {
       debugMessage := debugMessage # " " # "Transfer InvalidToken";
			return #err(#InvalidToken(request.token));
		};
    if (ExtCore.TokenIdentifier.getIndex(request.token) != 0) {
      debugMessage := debugMessage # " " # "Transfer InvalidToken";

			return #err(#InvalidToken(request.token));
		};
    
    let sender = ExtCore.User.toAID(request.from);
    let spender = AID.fromPrincipal(msg.caller, request.subaccount);
    let receiver = ExtCore.User.toAID(request.to);

    
    if (AID.equal(sender, spender) == false) {
        debugMessage := debugMessage # " " # "Transfer Unauthorized Spender = " # spender #" Sender=" # sender #  Principal.toText(msg.caller);
        return #err(#Unauthorized(spender));
    }; 

   
    switch (_balances.get(sender)) {
      case (?sender_balance) {
        if (sender_balance >= request.amount) {
          //Remove from sender first
          var sender_balance_new : Balance = sender_balance - request.amount;
          _balances.put(sender, sender_balance_new);
          
          var provisional_amount : Balance = request.amount;
          if (request.notify) {
            switch(ExtCore.User.toPrincipal(request.to)) {
              case (?canisterId) {
                let notifier : NotifyService = actor(Principal.toText(canisterId));
                switch(await notifier.tokenTransferNotification(request.token, request.from, request.amount, request.memo)) {
                  case (?balance) {
                    provisional_amount := balance;
                  };
                  case (_) {
                    var sender_balance_new2 = switch (_balances.get(sender)) {
                      case (?sender_balance) {
                          sender_balance + request.amount;
                      };
                      case (_) {
                          request.amount;
                      };
                    };
                    _balances.put(sender, sender_balance_new2);
                    Debug.print("Transfer Rejected");
                    return #err(#Rejected);
                  };
                };
              };
              case (_) {
                var sender_balance_new2 = switch (_balances.get(sender)) {
                  case (?sender_balance) {
                      sender_balance + request.amount;
                  };
                  case (_) {
                      request.amount;
                  };
                };
                _balances.put(sender, sender_balance_new2);
                return #err(#CannotNotify(receiver));
                Debug.print("Transfer CannotNotify");

              }
            };
          };
          assert(provisional_amount <= request.amount); //should never hit
          assert(provisional_amount <= request.amount); //should never hit

          debugMessage := debugMessage # " request.amount " # Nat.toText(request.amount) # "provisional_amount " # Nat.toText(provisional_amount);

          var receiver_balance_new = switch (_balances.get(receiver)) {
            case (?receiver_balance) {
                receiver_balance + provisional_amount;
            };
            case (_) {
                provisional_amount;
            };
          };
          _balances.put(receiver, receiver_balance_new);
          
          //Process sender refund
          if (provisional_amount < request.amount) {
            var sender_refund : Balance = request.amount - provisional_amount;
            var sender_balance_new2 = switch (_balances.get(sender)) {
              case (?sender_balance) {
                  sender_balance + sender_refund;
              };
              case (_) {
                  sender_refund;
              };
            };
            _balances.put(sender, sender_balance_new2);
          };
          
          return #ok(provisional_amount);
        } else {
          Debug.print("Transfer InsufficientBalance");
          return #err(#InsufficientBalance);
          
        };
      };
      case (_) {
        Debug.print("Transfer InsufficientBalance");

        return #err(#InsufficientBalance);
      };
    };
  };

  public query func extensions() : async [Extension] {
    EXTENSIONS;
  };
  public query func getOwner() : async Principal {
    init_owner;
  };  

  public query func getDebug() : async Text {
    debugMessage;
  };  
  public query func totalsupply() : async Nat {
    Nat.div(carrotsCirculation_,100000000);
  };  
  
  public query func balance_of (user: Principal) : async Nat 
  {
    let tokenHolder = AID.fromPrincipal(user, null);

    let currentBalance_ = _balances.get(tokenHolder);
    var currentBalance : Nat = 0;
    
    switch (currentBalance_) {
        case (?currentBalance_) {
            currentBalance := currentBalance_;
        };
        case (_) {
            currentBalance := 0;
        };
      };    
    return currentBalance;
  };

  public query func balance(request : BalanceRequest) : async BalanceResponse {
    let aid = ExtCore.User.toAID(request.user);

    switch (_balances.get(aid)) {
      case (?balance) {
        return #ok(balance);
      };
      case (_) {
        return #ok(0);
      };
    }
  };

  public query func supply(token : TokenIdentifier) : async Result.Result<Balance, CommonError> {
    #ok(_supply);
  };

  public query func metadata(token : TokenIdentifier) : async Result.Result<Metadata, CommonError> {
    #ok(METADATA);
  };
  
  public query func registry() : async [(AccountIdentifier, Balance)] {
    Iter.toArray(_balances.entries());
  };
  
  public func acceptCycles() : async () {
    let available = Cycles.available();
    let accepted = Cycles.accept(available);
    assert (accepted == available);
  };
  public query func availableCycles() : async Nat {
    return Cycles.balance();
  };
  public func wallet_receive() : async { accepted: Nat64 } 
  {
      let available = Cycles.available();
      let accepted = Cycles.accept(Nat.min(available, limit));
      { accepted = Nat64.fromNat(accepted) };
  };  
}
