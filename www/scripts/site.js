var port;

var lastData = {};
var state = STATE_NONE;

var STATE_NONE = 0;
var STATE_TURN = 1;
var STATE_END = 2;
var STATE_GEM_BACK = 3;
var STATE_SELECT_NOBLE = 4;
var selectedTokens = [0,0,0,0,0,0];

$(document).ready(function () {
    port = getParameterByName("port");
    $(".Card")
        .append("<div class='CardId'></div>")
        .hover(onCardOver, onCardOut)
        .click(onCardClick)
        .css({opacity: 0, display: "none"});
    
    $(".Deck").click(onDeckClick);
    $(".TokenContainer > a")
        .append("<div class='TokenSelected'></div><div class='TokenAvailable'></div><div class='TokenOverlay'></div>")
        .click(onTokenClick);
    
    $("#Test").click(function () {
        getState();
        return false;
    });
    
    $("#NewGame").click(function () {
        var players = "";
        for (var i=1;i<=4;i++) {
            players += "player" + i +"=" + $("#Player"+i+"Type").val() + "&";
        }
        $.get((port ? "http://127.0.0.1:" + port : "") + "/newGame?playerCount="+$("#PlayerCount").val()+"&"+players, function (data) {
            updateView(data);
        });
        return false;
    });

    $(".GemBackLink").click(onGemBackLinkClick);
    $(".GemForwardLink").click(onGemForwardLinkClick);
    $(".Send").click(onSendClick);
    $(".Next").click(onNextClick);
    $(".GemBackClose").click(onGemBackCloseClick);
    $(".Noble").click(onNobleClick);

    $("#PlayerCount").change(onPlayerCountChange);
    onPlayerCountChange();
    
    setTimeout(function () {
        getState();
    }, 150);
});

function getParameterByName(name) {
    name = name.replace(/[\[]/, "\\[").replace(/[\]]/, "\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results === null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

function onPlayerCountChange() {
    var pc = parseInt($("#PlayerCount").val());
    for (var i=0;i<4;i++) {
        $("#Player"+(i+1)+"Type").css({display: i<pc?"block":"none"});
    }
}

function onTokenClick() {
    if (state!=STATE_TURN || lastData.isEnded)
        return;
    if ($(this).hasClass("Token6"))
        return;
    if (selectedCard)
        $(selectedCard).trigger("click");
    var $sel = $(this).find(".TokenSelected");
    var $aval = $(this).find(".TokenAvailable");
    var self = this;

    var toZero = false;
    $(".TokenContainer > a").each(function () {
        if (this != self & parseInt($(this).attr("data-selected-count"))>0) {
            toZero = true;
        }
    });
    var a = parseInt($aval.text());
    var c = parseInt($sel.text());
    if (isNaN(c)) 
        c = 0;
    if (isNaN(a)) 
        a = 0;
    if (c==2 || !a || (toZero&&c==1) || (c==1&&a<3)) {
        a+=c;
        c = 0;
    } else {
        if (a) {
            c++;
            a--;
        }
    }
    $sel.text(c?c:"");
    $(this).attr("data-selected-count", c);
    $aval.text(a?a:"");
    if (c)
        $(this).addClass("Selected");
    else
        $(this).removeClass("Selected");
    var selExists = false;
    $(".TokenSelected").each(function () {
        if ($(this).text())
            selExists = true;
    });
    if (selExists)
        TweenLite.to(".Send", 0.2, {opacity:1, display: "block"});
    else
        TweenLite.to(".Send", 0.2, {opacity:0, display: "none"});
}

function getState() {
    $.get((port?"http://127.0.0.1:"+port:"")+"/webmethod?q=hello", function (data) {
        updateView(data);
    });
}

function onCardOver() {
    if (this == selectedCard || lastData.isEnded || state == STATE_SELECT_NOBLE)
        return;
    TweenLite.to(this, 0.3, {z: 15, opacity: 1});
}

function onCardOut() {
    if (this == selectedCard || lastData.isEnded || state == STATE_SELECT_NOBLE)
        return;
    TweenLite.to(this, 0.3, {z: 0, opacity: selectedCard?0.5:1});
}

function getCardData(id) {
    for (var i=0;i<lastData.cardData.length;i++) {
        
        if (lastData.cardData[i][0] == id) {
            return lastData.cardData[i].slice(1);
        }
    }
}

function canBuyCard(id) {
    var required = getCardData(id);
    var available = getTotalTokens();
    var canBuy = true;
    console.log(required, available);
    for (var i=0;i<5;i++) {
        if (required[i] > available[i])
            if (available[5] >= required[i]-available[i])
                available[5] -= required[i]-available[i];
            else
                canBuy = false;
    }
    return canBuy;
}

function getTotalTokens() {
    var a = []; 
    for (var i=0;i<6;i++)
        a[i] = lastData.players[0].gems[i] + lastData.players[0].bonuses[i];
    return a;
}

function getTotalGems() {
    var a = 0; 
    for (var i=0;i<6;i++)
        a += lastData.players[0].gems[i];
    return a;
}


var selectedCard;
function onCardClick() {
    if (lastData.isEnded || state == STATE_GEM_BACK || state == STATE_SELECT_NOBLE)
        return;
    
    $(".TokenContainer > a").removeClass("Selected").attr("data-selected-count", 0);
    $(".TokenContainer .TokenSelected").text("");
    $(".Card").css({zIndex: 2}).removeClass("Active");
    TweenLite.to(".Card", 0.3, {opacity: 0.4});
    $(this).css({zIndex: 10});
    $(selectedCard).css({zIndex: 5});
    if (selectedCard)
        TweenLite.to(selectedCard, 0.3, {x: 0, y: 0, z: 0,rotationX: 0});
    if ($(selectedCard).hasClass("Deck"))
        TweenLite.to(selectedCard, 0.3, {opacity: 0});
    var x = $(this).position().left + $(this).width()/2;
    var y = $(this).position().top + $(this).height()/2;
    var pw = $(this).parent().width();
    var ph = $(this).parent().height();
    var canBuy = canBuyCard($(this).attr("data-id"));
    if (this != selectedCard && (canBuy || !$(this).hasClass("Reserve"))) {
        $("#Board").css("z-index", 1000);
        if (!canBuy) {
            TweenLite.set(".GemGold", {x: 400, y: -400, z: 0});
            TweenLite.to(".GemGold", 0.3, {x: 0, y: 0, z: 0, opacity: 1, display: "block"});
        } 
        TweenLite.to(this, 0.3, {x: -x+pw*0.16, y: -y+ph*0.75, z: 0, opacity: 1, rotationX: -8});
        selectedCard = this;
        $(this).addClass("Active");
        TweenLite.to(".Send", 0.2, {opacity: 1, display: "block"});
    }
    else {
        $("#Board").css("z-index", "");
        selectedCard = null;
        TweenLite.to(".GemGold", 0.3, {x: 400, y: -400, z: 0, opacity: 0, display: "none"});
        TweenLite.to(".Card", 0.3, {opacity: 1});
        TweenLite.to(".Send", 0.2, {opacity: 0, display: "none"});
    }
}

function onDeckClick() {
    if (lastData.isEnded || state == STATE_GEM_BACK || state == STATE_SELECT_NOBLE)
        return;
    
    $(".TokenContainer > a").removeClass("Selected").attr("data-selected-count", 0);
    $(".TokenContainer .TokenSelected").text("");
    $(".Card, .Deck").css({zIndex: 2}).removeClass("Active");
    
    TweenLite.to(".Card", 0.3, {opacity: 0.4});
    $(this).css({zIndex: 10});
    $(selectedCard).css({zIndex: 5});
    if (selectedCard) {
        TweenLite.to(selectedCard, 0.3, {x: 0, y: 0, z: 0,rotationX: 0});
        if ($(selectedCard).hasClass("Deck"))
            TweenLite.to(selectedCard, 0.3, {opacity: 0});
    }
    var x = $(this).position().left;
    var y = $(this).position().top;
    var pw = $(this).parent().width();
    var ph = $(this).parent().height();
    if (this != selectedCard) {
        $("#Board").css("z-index", 1000);
        TweenLite.set(".GemGold", {x: 400, y: -400, z: 0});
        TweenLite.to(".GemGold", 0.3, {x: 0, y: 0, z: 0, opacity: 1, display: "block"});
        TweenLite.to(this, 0.3, {x: -x+pw*0.16, y: -y+ph*0.7, z: 0, opacity: 1, rotationX: -8});
        selectedCard = this;
        $(this).addClass("Active");
        TweenLite.to(".Send", 0.2, {opacity: 1, display: "block"});
    } 
    else { 
        $("#Board").css("z-index", "");
        selectedCard = null;
        TweenLite.to(this, 0.2,  {opacity: 0}); 
        TweenLite.to(".GemGold", 0.3, {x: 400, y: -400, z: 0, opacity: 0, display: "none"});
        TweenLite.to(".Card", 0.3, {opacity: 1});
        TweenLite.to(".Send", 0.2, {opacity: 0, display: "none"});
    }
}

function onNextClick() {
    $.get((port?"http://127.0.0.1:"+port:"")+"/nextStep", function (data) {
        updateView(data);
    });
}

function onSendClick() {
    var action = '';
    if (selectedCard) {
        if ($(selectedCard).hasClass("Deck")) {

            var backGems = checkExcessGems([0,0,0,0,0,1], 1);
            if (!backGems)
                return;
            action = "reserveCardFromDeck(" + (3-$(selectedCard).index()) +", ["+backGems+"])";
        }
        else {
            var id = $(selectedCard).attr("data-id");
            var canBuy = canBuyCard(id);
            if (canBuy) 
                action = "buyCard(" + id +")";
            else {
                var backGems = checkExcessGems([0,0,0,0,0,1], 1);
                if (!backGems)
                    return;
                action = "reserveCard(" + id +", ["+backGems+"])";
            }
        }
    } else {
        var isSelected = false;
        var selected = "[";
        var selectedIdx = [];
        var selectedGemCount = 0;
        var selectedGems = [];
        for (var i=1;i<=6;i++) {
            var c = parseInt($(".Token" + i).attr("data-selected-count"));
            selected += c + ",";
            selectedGems[i-1] = c;
            selectedGemCount += c;
            isSelected = isSelected | c>0;
        }

        var backGems = checkExcessGems(selectedGems, selectedGemCount);
        if (!backGems)
            return;
        
        state = STATE_TURN;
        selected = selected.substr(0, selected.length-1);
        selected += "]";
        if (isSelected)
            action = "getGems(" + selected + ",["+backGems+"])";
    }
    if (action) {
        $.get((port?"http://127.0.0.1:"+port:"")+"/performAction?action="+encodeURIComponent(action), function (data) {
            updateView(data);
        });
    }
}

function  checkExcessGems(selectedGems, selectedGemCount) {
    var totalGems = getTotalGems();

    var backGems = "0,0,0,0,0,0";
    if (state == STATE_GEM_BACK) {
        backGems = "";
        for (var i=0;i<6;i++) {
            backGems += getCurrentBackGem(i+1) + ",";
        }
        backGems = backGems.substr(0, backGems.length-1);
        TweenLite.to(".GemBackContainer", 0.3, {opacity: 0, display: "none"});
    }

    if (totalGems + selectedGemCount > 10 && state != STATE_GEM_BACK) {
        state = STATE_GEM_BACK;
        for (var i=0;i<6;i++) {
            var val = selectedGems[i] + lastData.players[0].gems[i];
            $(".GemContainer .Gem" + (i+1)).text(val?val:"");
        }
        $(".GemBackInfo span").text(totalGems + selectedGemCount - 10);
        $(".GemBackContainer .Gem span").text("");
        TweenLite.set(".Send", {opacity: 0, display: "none"});
        TweenLite.to(".GemBackContainer", 0.3, {opacity: 1, display: "block"});
        return false;
    }
    return backGems;
}

function updateView(data) {
    data = data.replace(/\r/g, "");
    data = data.replace(/\n/g, "\\n");
    data = JSON.parse(data);
    
    lastData = data;
    console.log(data.output);
    console.log(data);

    updateView2();
}
    
function updateView2() {
    var data = lastData;
    if (lastData.isEnded) {
        $("#Board").addClass("Ended");
        $(".Winners").text("Winner(s): " + lastData.winners);
        TweenLite.to(".ResultsContainer", 0.3, {opacity: 1, display: "block"});
        for (var i=0;i<4;i++) {
            var $cont = $(".PlayerResult" + (i+1)); 
            if (data.players[i]) {
                $cont.find(".PlayerName").text(data.players[i].name);
                $cont.find(".PlayerScore").text(": " + data.players[i].score);
            }
            else {
                $cont.find(".PlayerName").text("");
                $cont.find(".PlayerScore").text("");
            }
        }
    } else {
        $("#Board").removeClass("Ended");
        TweenLite.to(".ResultsContainer", 0.3, {opacity: 0, display: "none"});
    }
     
    $(".TokenSelected").text("");
    $(".TokenContainer > a").removeClass("Selected").attr("data-selected-count", 0);
    $(".Card, .Deck").removeClass("Active");
 
    TweenLite.set(".Deck", {opacity: 0});
    TweenLite.set(".Card", {opacity: 1});
    TweenLite.set(".Send", { opacity: 0, display: "none" });
    TweenLite.to(".GemGold", 0.3, {x: 400, y: -400, z: 0, opacity: 0, display: "none"});
    if (selectedCard) {
        if (!$(selectedCard).hasClass("Deck")) {
            if (!lastData.wrongAction) {
                TweenLite.set(selectedCard, { x: -1000, y: 0, z: 100, opacity: 1, rotationX: 0 });
            } else {
                TweenLite.to(".GemGold", 0.3, {x: 400, y: -400, z: 0, opacity: 0, display: "none"});
            }
            TweenLite.to(selectedCard, 0.6, { x: 0, y: 0, z: 0, opacity: 1, rotationX: 0 });
        }
        else
            TweenLite.to(selectedCard, 0.6, { x: 0, y: 0, z: 0, opacity: 0, rotationX: 0 });
            
        selectedCard = null;
        $("#Board").css("z-index", "");
    } 

    if (data.selectableNobles.length>0)
        state = STATE_SELECT_NOBLE;
    else
        state = STATE_TURN;
    
    TweenLite.set(".Card", {opacity: 0, display: "none"});
    for (var i = 0; i < 3; i++) {
        for (var j = 0; j < data.cards[i].length; j++) {
            var $card = $(".Card.CardRow" + (i + 1) + ".CardCol" + (j + 1));
            $card.attr("data-id", data.cards[i][j]);
            $card.find(".CardId").text(data.cards[i][j]);
            TweenLite.set($card, { opacity: 1, display: "block" });
        }
    }
    for (var i=0;i<4;i++) {
        if (data.players[i]) {
            var plStr = JSON.stringify(data.players[i]);
            $(".Score"+(i+1)).show().find("span").text(data.players[i].score);
            plStr= plStr.replace(/,"/g,",<br>&nbsp;&nbsp;&nbsp;\"");
            $(".Score"+(i+1)).find(".Info").html(plStr);
        } else
            $(".Score"+(i+1)).hide();
    }
    for (var i = 0; i < 6; i++) {
        $(".GemContainer .Gem.Gem" + (i+1)).text(data.players[0].gems[i]?data.players[0].gems[i]:"");
        $(".Bonus.Bonus" + (i+1)).text(data.players[0].bonuses[i]?data.players[0].bonuses[i]:"").css({opacity: data.players[0].bonuses[i]?1:0});
        $(".Token" + (i+1) + " .TokenAvailable").text(data.tokens[i]?data.tokens[i]:"").css({opacity: data.tokens[i]?1:0});
    }
    for (var i=0;i<data.players[0].reserves.length;i++) {
        $("#Reserve" + (i+1)).attr("data-id", data.players[0].reserves[i]);
        TweenLite.set("#Reserve" + (i+1), {opacity: 1, display: "block"});
    }
    for (var i=data.players[0].reserves.length;i<3;i++) {
        TweenLite.set("#Reserve" + (i+1), {opacity: 0, display: "none"});
    }
    var tl = new TimelineMax();
    var xOffset = $(".Card.CardRow1.CardCol1").position().left;
    $(".Card").each(function () {
        //var dx = cardCol1;
        var dx = xOffset - 200 - $(this).position().left;
        var t = -dx/500;
        //tl.from(this, t, { bezier: { curviness: 1, values: [{ x: 0, y: 0, z: 0 }, { x: 0, y: 0, z: 150 }, { x: dx, y: 0, z: 0 }] }, rotationY: -69, display: "none" , ease: Power3.easeOut}, "-=" + (t - 0.1));
    });
    for (var i=0;i<5;i++) {
        $(".Noble"+(i+1)).attr("data-id", data.nobles[i]?data.nobles[i][0]:"").removeClass("Selectable");
    }
    TweenLite.set(".Noble", {x: 0, y: 0, scale: 1});
    if (state == STATE_SELECT_NOBLE) {
        TweenLite.to(".Card", 0.3, {opacity: 0.3});
        for (var i=0;i<data.selectableNobles.length;i++) {
            var id = data.selectableNobles[i][0];
            var $noble = $(".Noble[data-id='"+id+"']");
            var h = $("#Board").height();
            $noble.addClass("Selectable");
            TweenLite.to($noble, 0.5, {scale: 1.4, x: -$("#Board").width()*0.4 - i*h/5, y: h/2-$noble.position().top});
        }
    }
    if ($("#Player1Type").find("option").length == 0) {
        for (var i=0;i<data.playerTypes.length-1;i++) {
            var name = data.playerTypes[i];
            for (var j=1;j<=4;j++)
                $("#Player"+j+"Type").append("<option value='"+name+"'>"+name+"</option>");
        }
    }
}
function getCurrentGem(idx) {
    var a = parseInt($(".GemContainer .Gem" + (idx)).text());
    return isNaN(a)?0:a;
}
function getCurrentBackGem(idx) {
    var a = parseInt($(".GemBackContainer .Gem" + (idx)).text());
    return isNaN(a)?0:a;
}
function setCurrentGem(idx, val) {
    $(".GemContainer .Gem" + (idx)).text(val?val:"");
}
function setCurrentBackGem(idx,val) {
    $(".GemBackContainer .Gem" + (idx) + " span").text(val?val:"");
}
function onGemBackLinkClick() {
    var idx = $(this).parent().index();
    var totalGem = 0;
    var gemCount = getCurrentGem(idx);
    var backGemCount = getCurrentBackGem(idx);
    for (var i=1;i<=6;i++) {
        var c = getCurrentGem(i);
        totalGem += c;
        console.log(i,c);
    }
    var excessGem = totalGem - 10;
    if (excessGem>0) {
        if (gemCount>0) {
            setCurrentGem(idx, gemCount-1);
            setCurrentBackGem(idx, backGemCount+1);
            excessGem--;
            $(".GemBackInfo span").text(excessGem);
        }
    }
    TweenLite.set(".Send", {opacity: excessGem>0?0:1, display: excessGem>0?"none":"block"});
    return false;
}

function onGemForwardLinkClick() {
    var idx = $(this).parent().index();
    var totalGem = 0;
    var gemCount = getCurrentGem(idx);
    var backGemCount = getCurrentBackGem(idx);
    for (var i=1;i<=6;i++) {
        totalGem += getCurrentGem(i);
    } 
    var excessGem = totalGem - 10;
    if (backGemCount>0) {
        setCurrentGem(idx, gemCount+1);
        setCurrentBackGem(idx, backGemCount-1);
        excessGem++;
        $(".GemBackInfo span").text(excessGem);
    }
    TweenLite.set(".Send", {opacity: excessGem>0?0:1, display: excessGem>0?"none":"block"});
    return false;
}

function onGemBackCloseClick() {
    updateView2();
    TweenLite.to(".GemBackContainer", 0.3, {opacity: 0, display: "none"});
    return false;
}

function onNobleClick() {
    if ($(this).hasClass("Selectable")) {
        $.get((port?"http://127.0.0.1:"+port:"")+"/performAction?action="+encodeURIComponent("getNoble("+$(this).attr("data-id")+")"), function (data) {
            updateView(data);
        });
    }
}