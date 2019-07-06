
// MIT License

// Copyright (c) 2019 Tsukanov Alexander

// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// Транслятор

Var Result;       // array
Var OffsetMap;    // map[number]
Var CallIndex;    // number
Var FuncVars;     // map[Sign](number)
Var CallSites;    // map[Sign](array)

Var LeftFP;       // number const

Var LastReturn;   // boolean

Var Labels;       // map[string](number)
Var GotoList;     // map[string]array(number)

Var Nodes;        // enum
Var Tokens;       // enum
Var Operators;    // structure as map[one of Tokens](string)

Var Scope;        // structure

Procedure Init(BSLParser) Export	
	Operators = New Structure(
		"Eql, Neq, Lss, Gtr, Leq, Geq, Add, Sub, Mul, Div, Mod, Or, And, Not",
		"=", "<>", "<", ">", "<=", ">=", "+", "-", "*", "/", "%", " Or ", " And ", "Not "
	);
	Nodes = BSLParser.Nodes();
	Tokens = BSLParser.Tokens();
	Result = New Array;
	OffsetMap = New Map;
	CallIndex = 0;
	FuncVars = New Map;
	CallSites = New Map;
	Labels = New Map;
	GotoList = New Map;
	LeftFP = 3;
	LastReturn = False;
EndProcedure // Init()

Function Hooks() Export
	Var Hooks;
	Hooks = New Array;
	Hooks.Add("VisitModule");
	Return Hooks;
EndFunction // Hooks()

Function Result() Export
	Return Result;
EndFunction // Refult()

Procedure OpenScope()
	Scope = New Structure("Outer, Break, Continue", Scope, New Array, New Array);
EndProcedure // OpenScope()

Procedure CloseScope()
	Scope = Scope.Outer;
EndProcedure // CloseScope()

Procedure VisitModule(Module, M = Undefined, Counters = Undefined) Export
	
	Address = 0;
	
	Result.Add("");
	
	For Each Decl In Module.Decls Do
		
		Type = Decl.Type;
		
		If Type = Nodes.VarModListDecl Then		
			
			For Each VarDecl In Decl.List Do
				Address = Address + 1;
				OffsetMap[VarDecl] = Address;
			EndDo;
			
		ElsIf Type = Nodes.MethodDecl Then 
			
			Sign = Decl.Sign;

			ItemsCount = 1; // one item for FP
			
			For Index = 0 To Sign.Params.UBound() Do
				OffsetMap[Sign.Params[Index]] = ItemsCount;
				ItemsCount = ItemsCount + 2;
			EndDo;
			For Index = 0 To Decl.Vars.UBound() Do
				OffsetMap[Decl.Vars[Index]] = ItemsCount;
				ItemsCount = ItemsCount + 1;
			EndDo;
			For Index = 0 To Decl.Auto.UBound() Do
				OffsetMap[Decl.Auto[Index]] = ItemsCount;
				ItemsCount = ItemsCount + 1;
			EndDo;		
			
			FuncVars[Sign] = Decl.Vars.Count() + Decl.Auto.Count();
			
		Else
			// error
		EndIf;
		
	EndDo;
	
	For Each Decl In Module.Decls Do
		
		Type = Decl.Type;
		
		If Type = Nodes.MethodDecl Then 
			
			Sign = Decl.Sign;
			OffsetMap[Sign] = Result.Count();
			Result.Add("// Method " + Sign.Name);
			
			VisitStatements(Decl.Body);
			
			// эпилог
			If Not LastReturn Then
				Result.Add("For _=FP+1 To SP Do M[_]=Undefined EndDo;SP=FP-1;FP=M[FP];IP=M[SP];M[SP]=Undefined;SP=SP-1;RP=M[SP];M[SP]=Undefined;SP=SP-1; // GC, Restore FP, Return, del ret addr");
			EndIf; 
			
		EndIf;
		
	EndDo;
	
	Code = Result.Count();
	Result.Add("// Module");
	
	ItemsCount = 0;
	For Index = 0 To Module.Auto.UBound() Do
		ItemsCount = ItemsCount + 1;
		OffsetMap[Module.Auto[Index]] = ItemsCount;
	EndDo;
	
	Result.Add(StrTemplate("FP=%1; SP=%2; RP=%2;", Address, Address + ItemsCount));
	
	VisitStatements(Module.Body);
	
	If ItemsCount > 0 Then
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC mod vars", ItemsCount-1, ItemsCount));
	EndIf; 
	
	For Each CallSite In CallSites Do
		For Each Index In CallSite.Value Do
			Result[Index] = StrTemplate("IP=%1; // Call %2()", Fmt(OffsetMap[CallSite.Key]), CallSite.Key.Name);
		EndDo; 
	EndDo; 
	
	Result[0] = StrTemplate("IP=%1;", Fmt(Code));
	
	Result.Add("// End");
	
EndProcedure // VisitModule()

Procedure VisitStatements(Statements)
	For Each Stmt In Statements Do
		VisitStmt(Stmt);
	EndDo;
	For Each Item In GotoList Do
		For Each Addr In Item.Value Do
			Result[Addr] = StrTemplate("IP=%1; // goto", Fmt(Labels[Item.Key]));
		EndDo; 
	EndDo; 
EndProcedure // VisitStatements()

#Region VisitStmt

Procedure VisitStmt(Stmt)
	Type = Stmt.Type;	
	LastReturn = False;
	If Type = Nodes.AssignStmt Then
        VisitAssignStmt(Stmt);
    ElsIf Type = Nodes.ReturnStmt Then
        VisitReturnStmt(Stmt);
		LastReturn = True;
    ElsIf Type = Nodes.BreakStmt Then
        VisitBreakStmt(Stmt);
    ElsIf Type = Nodes.ContinueStmt Then
        VisitContinueStmt(Stmt);
    ElsIf Type = Nodes.RaiseStmt Then
        VisitRaiseStmt(Stmt);
    ElsIf Type = Nodes.ExecuteStmt Then
        VisitExecuteStmt(Stmt);
    ElsIf Type = Nodes.CallStmt Then
        VisitCallStmt(Stmt);
    ElsIf Type = Nodes.IfStmt Then
        VisitIfStmt(Stmt);
    ElsIf Type = Nodes.WhileStmt Then
        VisitWhileStmt(Stmt);
    ElsIf Type = Nodes.ForStmt Then
        VisitForStmt(Stmt);
    ElsIf Type = Nodes.ForEachStmt Then
        VisitForEachStmt(Stmt);
    ElsIf Type = Nodes.TryStmt Then
        VisitTryStmt(Stmt);
    ElsIf Type = Nodes.GotoStmt Then
        VisitGotoStmt(Stmt);
    ElsIf Type = Nodes.LabelStmt Then
        VisitLabelStmt(Stmt);
	Else
		// error
	EndIf;
EndProcedure // VisitStmt()

Procedure VisitAssignStmt(AssignStmt)
	Var Left, Right;
	Decl = AssignStmt.Left.Head.Decl;
	Buffer = New Array; 
	VisitIdentExpr(AssignStmt.Left, Buffer);
	Left = StrConcat(Buffer);
	Buffer = New Array;
	CallIndex = 0;
	VisitExpr(AssignStmt.Right, Buffer);
	Right = StrConcat(Buffer);
	Result.Add(StrTemplate("%1=%2; // Assign", Left, Right));
	If CallIndex > 0 Then // очистка результатов функций
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
		CallIndex = 0;
	EndIf; 
EndProcedure // VisitAssignStmt()

Procedure VisitCallStmt(CallStmt)
	Buffer = New Array;
	CallIndex = 0;
	VisitIdentExpr(CallStmt.Ident, Buffer);
	If CallStmt.Ident.Head.Decl = Undefined
		Or CallStmt.Ident.Args = Undefined Then
		Buffer.Add(";");
		Result.Add(StrConcat(Buffer));
		If CallIndex > 0 Then // очистка результатов функций
			Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after call", CallIndex-1, CallIndex));
			CallIndex = 0;
		EndIf;
	Else
		If CallIndex > 0 Then // очистка результатов функций
			Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after call", CallIndex-1, CallIndex));
			CallIndex = 0;
		EndIf;
	EndIf;
EndProcedure // VisitCallStmt()

Procedure VisitReturnStmt(ReturnStmt)
	If ReturnStmt.Expr <> Undefined Then
		Buffer = New Array;
		CallIndex = 0;
		VisitExpr(ReturnStmt.Expr, Buffer);
		Right = StrConcat(Buffer);
		Result.Add(StrTemplate("M[FP-%1]=%2; // Return", LeftFP, Right));
		If CallIndex > 0 Then // очистка результатов функций
			Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after return", CallIndex-1, CallIndex));
			CallIndex = 0;
		EndIf;
		// эпилог как в конце функции
		Result.Add("For _=FP+1 To SP Do M[_]=Undefined EndDo;SP=FP-1;FP=M[FP];IP=M[SP];M[SP]=Undefined;SP=SP-1;RP=M[SP];M[SP]=Undefined;SP=SP-1; // GC, Restore FP, Return, del ret addr");
	EndIf;
EndProcedure // VisitReturnStmt()

Procedure VisitBreakStmt(BreakStmt)
	Result.Add();
	Scope.Break.Add(Result.UBound());
EndProcedure // VisitBreakStmt()

Procedure VisitContinueStmt(ContinueStmt)
	Result.Add();
	Scope.Continue.Add(Result.UBound());
EndProcedure // VisitContinueStmt()

Procedure VisitRaiseStmt(RaiseStmt)
	If RaiseStmt.Expr <> Undefined Then
		Buffer = New Array;
		VisitExpr(RaiseStmt.Expr, Buffer);
		Result.Add(StrTemplate("Raise %1;", StrConcat(Buffer)));
	Else
		Result.Add("Raise;");
	EndIf;
EndProcedure // VisitRaiseStmt()

Procedure VisitExecuteStmt(ExecuteStmt)
	Buffer = New Array;
	Buffer.Add("Execute ");
	VisitExpr(ExecuteStmt.Expr, Buffer);
	Buffer.Add(";");
	Result.Add(StrConcat(Buffer));
EndProcedure // VisitExecuteStmt()

Procedure VisitIfStmt(IfStmt)
	
	CallIndex = 0;
	
	Ends = New Array;
	List = New Array;
		
	Buffer = New Array;	
	VisitExpr(IfStmt.Cond, Buffer);
	Result.Add(StrConcat(Buffer)); // deferred	
	Item = New Structure("Addr, CallIndex, Goto", Result.UBound(), CallIndex);
	List.Add(Item);
		
	If CallIndex > 0 Then // очистка результатов функций
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
		CallIndex = 0;
	EndIf;
	
	VisitStatements(IfStmt.Then);	
	Result.Add(); Ends.Add(Result.UBound()); // deferred
	
	ElsIfList = IfStmt.ElsIf;
	If ElsIfList <> Undefined Then
		For Index = 0 To ElsIfList.UBound() Do
			
			ElsIfStmt = ElsIfList[Index];		
			
			Item.Goto = Result.UBound();
			
			Buffer = New Array;
			VisitExpr(ElsIfStmt.Cond, Buffer);			
			Result.Add(StrConcat(Buffer)); // deferred
			Item = New Structure("Addr, CallIndex, Goto", Result.UBound(), CallIndex);
			List.Add(Item); 
			
			If CallIndex > 0 Then // очистка результатов функций
				Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
				CallIndex = 0;
			EndIf;
			
			VisitStatements(ElsIfStmt.Then);
			Result.Add(); Ends.Add(Result.UBound()) // deferred
			
		EndDo; 
	EndIf;
	
	If IfStmt.Else <> Undefined Then
		Item.Goto = Result.UBound();
		VisitStatements(IfStmt.Else.Body);
	EndIf; 
	
	For Each Item In List Do
		If Item.Goto = Undefined Then
			Item.Goto = Fmt(Result.UBound());
		EndIf; 
		If Item.CallIndex > 0 Then
			GC = StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2;", Item.CallIndex-1, Item.CallIndex);
			Result[Item.Addr] = StrTemplate("If Not (%1) Then IP=%2; %3 EndIf; // GC calls after expr", Result[Item.Addr], Fmt(Item.Goto), GC);
		Else
			Result[Item.Addr] = StrTemplate("If Not (%1) Then IP=%2 EndIf;", Result[Item.Addr], Fmt(Item.Goto));
		EndIf; 
	EndDo; 
	
	GotoEnd = StrTemplate("IP=%1;", Fmt(Result.UBound()));
	For Each Index In Ends Do
		Result[Index] = GotoEnd;
	EndDo; 
	
EndProcedure // VisitIfStmt()

Procedure VisitWhileStmt(WhileStmt)
	
	OpenScope();
	
	CallIndex = 0;
	
	HeadAddr = Result.UBound();
	
	Buffer = New Array;
	VisitExpr(WhileStmt.Cond, Buffer);
	Result.Add();
	ExprAddr = Result.UBound(); 
	
	If CallIndex > 0 Then // очистка результатов функций
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
		CallIndex = 0;
	EndIf;
	
	VisitStatements(WhileStmt.Body);
	
	Result.Add(StrTemplate("IP=%1;", Fmt(HeadAddr)));
	Result[ExprAddr] = StrTemplate("If Not (%1) Then IP=%2 EndIf;", StrConcat(Buffer), Fmt(Result.UBound()));
	
	For Each Addr In Scope.Break Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(Result.UBound()));
	EndDo;
	Scope.Break.Clear();
	
	For Each Addr In Scope.Continue Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(HeadAddr));
	EndDo;
	Scope.Continue.Clear();
	
	CloseScope();
	
EndProcedure // VisitWhileStmt()

Procedure VisitForStmt(ForStmt)
	
	OpenScope();
	
	CallIndex = 0;
	
	Buffer = New Array;
	VisitIdentExpr(ForStmt.Ident, Buffer);
	Left = StrConcat(Buffer);
	
	Buffer = New Array;
	VisitExpr(ForStmt.From, Buffer);
	Right = StrConcat(Buffer);
	
	Result.Add(StrTemplate("%1=%2; // Init", Left, Right));
	HeadAddr = Result.UBound();
	
	Buffer = New Array;
	VisitExpr(ForStmt.To, Buffer);
	Result.Add();
	ExprAddr = Result.UBound();	
	
	If CallIndex > 0 Then // очистка результатов функций
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
		CallIndex = 0;
	EndIf;
	
	VisitStatements(ForStmt.Body);
	
	Result.Add(StrTemplate("%1=%1+1;IP=%2;", Left, HeadAddr));
	Result[ExprAddr] = StrTemplate("If %1 > %2 Then IP=%3 EndIf;", Left, StrConcat(Buffer), Fmt(Result.UBound()));
	
	For Each Addr In Scope.Break Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(Result.UBound()));
	EndDo;
	Scope.Break.Clear();
	
	For Each Addr In Scope.Continue Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(HeadAddr));
	EndDo;
	Scope.Continue.Clear();
	
	CloseScope();
	
EndProcedure // VisitForStmt()

Procedure VisitForEachStmt(ForEachStmt)
	
	OpenScope();
	
	CallIndex = 0;
	
	Buffer = New Array;
	VisitIdentExpr(ForEachStmt.Ident, Buffer);
	Left = StrConcat(Buffer);
	
	Buffer = New Array;
	VisitExpr(ForEachStmt.In, Buffer);
	Result.Add(StrTemplate("A=New Array;For Each X In %1 Do A.Add(X) EndDo;SP=SP+1;M[SP]=A;SP=SP+1;M[SP]=A.UBound();SP=SP+1;M[SP]=0;A=Undefined;X=Undefined;", StrConcat(Buffer)));	
	HeadAddr = Result.UBound();
	
	Result.Add();
	ExprAddr = Result.UBound();		
	
	If CallIndex > 0 Then // очистка результатов функций
		Result.Add(StrTemplate("For _=SP-%1 To SP Do M[_]=Undefined EndDo;SP=SP-%2; // GC calls after assign", CallIndex-1, CallIndex));
		CallIndex = 0;
	EndIf;
	
	VisitStatements(ForEachStmt.Body);
	
	Result.Add(StrTemplate("IP=%1;", HeadAddr));
	Result[ExprAddr] = StrTemplate("If M[SP] > M[SP-1] Then IP=%1 Else %2=M[SP-2][M[SP]]; M[SP]=M[SP]+1 EndIf;", Fmt(Result.UBound()), Left);
	
	For Each Addr In Scope.Break Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(Result.UBound()));
	EndDo;
	Scope.Break.Clear();
	
	For Each Addr In Scope.Continue Do
		Result[Addr] = StrTemplate("IP=%1;", Fmt(HeadAddr));
	EndDo;
	Scope.Continue.Clear();
		
	Result.Add("SP=SP-3;For _=SP+1 TO SP+3 Do M[_]=Undefined EndDo;");
	
	CloseScope();
	
EndProcedure // VisitForEachStmt()

Procedure VisitTryStmt(TryStmt)
	
	Result.Add();
	HeadAddr = Result.UBound();
	VisitStatements(TryStmt.Try);
	
	Result.Add();
	JumpAddr = Result.UBound();
	
	Result[HeadAddr] = StrTemplate("EP=EP+1;ES[EP]=%1;EP=EP+1;ES[EP]=SP;", Fmt(Result.UBound()+1));
	VisitStatements(TryStmt.Except.Body);
	
	Result.Add("ES[EP]=Undefined;EP=EP-1;ES[EP]=Undefined;EP=EP-1;");
	Result[JumpAddr] = StrTemplate("IP=%1;", Fmt(Result.UBound()));
	
EndProcedure // VisitTryStmt()

// TODO: нужны области видимости для меток
Procedure VisitGotoStmt(GotoStmt)
	Result.Add();
	List = GotoList[GotoStmt.Label];
	If List = Undefined Then
		List = New Array;
		GotoList[GotoStmt.Label] = List;
	EndIf;
	List.Add(Result.UBound());
EndProcedure // VisitGotoStmt()

Procedure VisitLabelStmt(LabelStmt)
	Labels[LabelStmt.Label] = Result.UBound();
EndProcedure // VisitLabelStmt()

#EndRegion // VisitStmt

#Region VisitExpr

Procedure VisitExpr(Expr, Buffer)
    Var Type, Hook;
	Type = Expr.Type;
	If Type = Nodes.BasicLitExpr Then
        VisitBasicLitExpr(Expr, Buffer);
    ElsIf Type = Nodes.IdentExpr Then
        VisitIdentExpr(Expr, Buffer);
    ElsIf Type = Nodes.UnaryExpr Then
        VisitUnaryExpr(Expr, Buffer);
    ElsIf Type = Nodes.BinaryExpr Then
        VisitBinaryExpr(Expr, Buffer);
    ElsIf Type = Nodes.NewExpr Then
        VisitNewExpr(Expr, Buffer);
    ElsIf Type = Nodes.TernaryExpr Then
        VisitTernaryExpr(Expr, Buffer);
    ElsIf Type = Nodes.ParenExpr Then
        VisitParenExpr(Expr, Buffer);
    ElsIf Type = Nodes.NotExpr Then
        VisitNotExpr(Expr, Buffer);
    ElsIf Type = Nodes.StringExpr Then
        VisitStringExpr(Expr, Buffer);
	EndIf;
EndProcedure // VisitExpr()

Procedure VisitBasicLitExpr(BasicLitExpr, Buffer)
	BasicLitKind = BasicLitExpr.Kind;
	If BasicLitKind = Tokens.String Then
		Buffer.Add(BasicLitExpr.Value);
	ElsIf BasicLitKind = Tokens.StringBeg Then
		Buffer.Add(BasicLitExpr.Value);
	ElsIf BasicLitKind = Tokens.StringMid Then
		Buffer.Add(BasicLitExpr.Value);
	ElsIf BasicLitKind = Tokens.StringEnd Then
		Buffer.Add(BasicLitExpr.Value);
	ElsIf BasicLitKind = Tokens.Number Then
		Buffer.Add(Fmt(BasicLitExpr.Value));
	ElsIf BasicLitKind = Tokens.DateTime Then
		Buffer.Add(StrTemplate("'%1'", Format(BasicLitExpr.Value, "DF=yyyyMMddhhmmss; DE=00010101")));
	ElsIf BasicLitKind = Tokens.True Then
		Buffer.Add("True");
	ElsIf BasicLitKind = Tokens.False Then
		Buffer.Add("False");
	ElsIf BasicLitKind = Tokens.Undefined Then
		Buffer.Add("Undefined");
	ElsIf BasicLitKind = Tokens.Null Then
		Buffer.Add("Null");
	Else
		Raise "Unknown basic literal";
	EndIf;
EndProcedure // VisitBasicLitExpr()

Procedure VisitIdentExpr(IdentExpr, Buffer, ArgByRef = False)	
	
	Decl = IdentExpr.Head.Decl;
	
	If IdentExpr.Args <> Undefined Then		
			
		GenerateCall(IdentExpr, Buffer);
		
	ElsIf Decl = Undefined Then
		
		Buffer.Add(IdentExpr.Head.Name);
		
	Else
		
		Offset = OffsetMap[Decl];
		Type = Decl.Type;
		
		If Type = Nodes.VarModDecl Then
			Buffer.Add(StrTemplate("M[%1]", Fmt(Offset)));	
		ElsIf Type = Nodes.ParamDecl And Not Decl.ByVal Then	
			Buffer.Add(StrTemplate("M[FP+%1][M[FP+%2]]", Fmt(Offset), Fmt(Offset+1)));	 
		Else	
			Buffer.Add(StrTemplate("M[FP+%1]", Fmt(Offset)));
		EndIf; 
		
	EndIf;
		
	VisitTail(IdentExpr.Tail, Buffer);
	
EndProcedure // VisitIdentExpr()

Procedure GenerateCall(IdentExpr, Buffer)
	
	Head = IdentExpr.Head;
	Decl = Head.Decl;
	Args = IdentExpr.Args;
	
	If Decl = Undefined Then
		
		// Вызов нативного метода, т.к. объявление не обнаружено
		
		Buffer.Add(Head.Name);
		If Head.Name <> "ErrorInfo" Then
			Buffer.Add("(");
			VisitExprList(Args, Buffer);
			Buffer.Add(")");
		EndIf; 
		
	Else 
		
		// Вызов интерпретируемого метода 
		
		Params = Decl.Params;
		ItemsCount = Params.Count()*2 + FuncVars[Decl];
		
		// буфер для пролога
		Prolog = New Array;
		
		// буфер для промежуточных значений, передаваемых в параметры по ссылке
		Values = New Array;
		
		// смещение текущего значения относительно CP
		ValueOffset = 0;
		
		// генерация пролога {{
		
		// кадр вызова:
		// -2 Результат функции (для процедур тоже, но игнорится)
		// -1 Адрес возврата
		//  0 Адрес кадра родительской функции
		// +1 Аргумент1.1 (значение или память или объект)
		// +2 Аргумент1.2 (неопределено или имя поля или индекс)
		// +3 Аргумент2.1 (значение или память или объект)
		// +4 Аргумент2.2 (неопределено или имя поля или индекс)
		// +5 Переменная1 (значение)
		// +6 Переменная2 (значение)
		// +N {значения передаваемые в параметры по ссылке};
		
		// ячейка под результат функции
		Prolog.Add("SP=SP+1;");
		
		// фиксация базы смещений промежуточных значений
		Prolog.Add(StrTemplate("TP=SP+%1;", Fmt(LeftFP + ItemsCount)));
		
		// сохранение указателя инструкции и указателя кадра для возврата
		Prolog.Add("SP=SP+1;M[SP]=RP;SP=SP+1;M[SP]=IP+1;SP=SP+1;M[SP]=FP;;"); 
		
		Index = 0;
		
		TotalArgs = Args.Count();
		
		For Index = 0 To Params.UBound() Do
			
			// формальный параметр
			Param = Params[Index];
			
			// значение соответствующего аргумента
			If Index < TotalArgs Then
				ArgExpr = Args[Index];
			Else // аргумент в конце опущен (TODO: нужно генерить ошибку если он не имеет значения по умолчанию)
			    ArgExpr = Undefined;
			EndIf;
			
			If ArgExpr = Undefined Then
				
				// аргумент либо пропущен, либо опущен - нужно взять значение по умолчанию
				ArgExpr = Param.Value; 
				
				If ArgExpr = Undefined Then
					// аргумент не имеет значения по умолчанию - значит Неопределено
					ArgExpr = New Structure("Type, Kind, Value", "BasicLitExpr", Tokens.Undefined, Undefined); 
				EndIf;
				
			EndIf;
			
			Prolog.Add("SP=SP+1;M[SP]=");
			
			If ArgExpr.Type = Nodes.IdentExpr Then
				
				If ArgExpr.Args = Undefined Then 
					
					ArgDecl = ArgExpr.Head.Decl;
					ArgOffset = OffsetMap[ArgDecl];
					
					If Param.ByVal Then
						
						// по значению
						
						VisitIdentExpr(ArgExpr, Prolog);
						Prolog.Add(";SP=SP+1;M[SP]=Undefined");
						
					ElsIf ArgExpr.Tail.Count() > 0 Then
						
						// по ссылке с хвостом
						
						If ArgDecl.Type = Nodes.VarModDecl Then
							Prolog.Add(StrTemplate("M[%1]", Fmt(ArgOffset)));
						ElsIf ArgDecl.Type = "ParamDecl" And Not ArgDecl.ByVal Then
							Prolog.Add(StrTemplate("M[FP+%1][M[FP+%2]]", Fmt(ArgOffset), Fmt(ArgOffset+1)));
						Else	
							Prolog.Add(StrTemplate("M[FP+%1]", Fmt(ArgOffset)));
						EndIf;
						
						TipOfTail = VisitTailRef(ArgExpr.Tail, Prolog);
						Prolog.Add(";SP=SP+1;M[SP]=" + TipOfTail);
						
					Else
						
						// по ссылке без хвоста
						
						If ArgDecl.Type = Nodes.VarModDecl Then
							Prolog.Add(StrTemplate("M;SP=SP+1;M[SP]=%1", Fmt(ArgOffset)));
						ElsIf ArgDecl.Type = "ParamDecl" And Not ArgDecl.ByVal Then
							Prolog.Add(StrTemplate("M[FP+%1];SP=SP+1;M[SP]=M[FP+%2]", Fmt(ArgOffset), Fmt(ArgOffset+1)));
						Else	
							Prolog.Add(StrTemplate("M;SP=SP+1;M[SP]=FP+%1", Fmt(ArgOffset)));
						EndIf;
						
					EndIf;
					
				Else // вызов функции
					
					If Param.ByVal Then
						
						// по значению
					
						GenerateCall(ArgExpr, Prolog);
						Prolog.Add(";SP=SP+1;M[SP]=Undefined");
						
					ElsIf ArgExpr.Tail.Count() > 0 Then
						
						GenerateCall(ArgExpr, Prolog);
						
						TipOfTail = VisitTailRef(ArgExpr.Tail, Prolog);
						Prolog.Add(";SP=SP+1;M[SP]=" + TipOfTail);
						
					Else
						
						// по ссылке без хвоста
						
						// промежуточные значения, передаваемые по ссылке, размещаются подобно переменным
						
						ValueOffset = ValueOffset + 1;
						Prolog.Add("M;SP=SP+1;M[SP]=TP+" + Fmt(ValueOffset));
						
						Values.Add("SP=SP+1;M[SP]=");
						GenerateCall(ArgExpr, Values);
						Values.Add(";");
						
						// может быть делать ссылки на сами результаты функций? O_o
						
					EndIf; 
						
				EndIf; 
				
			ElsIf Not Param.ByVal Then
				
				// промежуточные значения, передаваемые по ссылке, размещаются подобно переменным
				
				ValueOffset = ValueOffset + 1;
				Prolog.Add("M;SP=SP+1;M[SP]=TP+" + Fmt(ValueOffset));
				
				Values.Add("SP=SP+1;M[SP]=");
				VisitExpr(ArgExpr, Values);
				Values.Add(";");
				
			Else
				
				VisitExpr(ArgExpr, Prolog);
				Prolog.Add(";SP=SP+1;M[SP]=Undefined");
				
			EndIf; 

			Prolog.Add(";");
			
		EndDo;
		
		// Резервирование места под локальные переменные
		Prolog.Add(StrTemplate("SP=SP+%1;", Fmt(FuncVars[Decl])));
		
		// Размещение промежуточных значений
		If Values.Count() > 0 Then
			Prolog.Add(StrConcat(Values) + ";");
		EndIf; 
		
		Prolog.Add("RP=SP;");
		
		// Установка указателя кадра вызываемой процедуры
		Prolog.Add(StrTemplate("FP=SP-%1;", Fmt(ItemsCount + ValueOffset)));
		Prolog.Add(" // allocate: 1 Result, vals ptr, ret IP, parent FP, N args, N vars, N vals, New RP, New FP");
		
		Result.Add(StrConcat(Prolog));
		
		// вызов
		Result.Add(); // deferred
		List = CallSites[Decl];
		If List = Undefined Then
			List = New Array;
			CallSites[Decl] = List;
		EndIf; 
		List.Add(Result.UBound());
		
		// }} генерация пролога
		
		// нужно вернуть в выражение результат вызова функции
		// адрес результата берется как смещение относительно RP
		CallIndex = CallIndex + 1;
		Buffer.Add(StrTemplate("M[RP+%1]", CallIndex));
		
	EndIf; 
	
EndProcedure

Procedure VisitUnaryExpr(UnaryExpr, Buffer)
	Buffer.Add(StrTemplate("%1", Operators[UnaryExpr.Operator]));
	VisitExpr(UnaryExpr.Operand, Buffer);
EndProcedure // VisitUnaryExpr()

Procedure VisitBinaryExpr(BinaryExpr, Buffer)
	VisitExpr(BinaryExpr.Left, Buffer);
	Buffer.Add(Operators[BinaryExpr.Operator]);
	VisitExpr(BinaryExpr.Right, Buffer);
EndProcedure // VisitBinaryExpr()

Procedure VisitNewExpr(NewExpr, Buffer)
	Buffer.Add("New ");
	If NewExpr.Name <> Undefined Then
		Buffer.Add(NewExpr.Name);
	EndIf;
	If NewExpr.Args.Count() > 0 Then
		Buffer.Add("(");
		VisitExprList(NewExpr.Args, Buffer);
		Buffer.Add(")");
	EndIf;
EndProcedure // VisitNewExpr()

Procedure VisitTernaryExpr(TernaryExpr, Buffer)
	Buffer.Add("?(");
	VisitExpr(TernaryExpr.Cond, Buffer);
	Buffer.Add(", ");
	VisitExpr(TernaryExpr.Then, Buffer);
	Buffer.Add(", ");
	VisitExpr(TernaryExpr.Else, Buffer);
	Buffer.Add(")");
	VisitTail(TernaryExpr.Tail, Buffer);
EndProcedure // VisitTernaryExpr()

Procedure VisitParenExpr(ParenExpr, Buffer)
	Buffer.Add("(");
	VisitExpr(ParenExpr.Expr, Buffer);
	Buffer.Add(")");
EndProcedure // VisitParenExpr()

Procedure VisitNotExpr(NotExpr, Buffer)
	Buffer.Add("Not ");
	VisitExpr(NotExpr.Expr, Buffer);
EndProcedure // VisitNotExpr()

Procedure VisitStringExpr(StringExpr, Buffer)
	List = StringExpr.List;
	Buffer.Add("""");
	Buffer.Add(List[0].Value);
	Buffer.Add("""");
	For Index = 1 To List.UBound() Do
		Expr = List[Index];
		Buffer.Add(" """);
		Buffer.Add(Expr.Value);
		Buffer.Add("""");
	EndDo;
EndProcedure // VisitStringExpr()

#EndRegion // VisitExpr

#Region Aux

Function Fmt(Value)
	Return Format(Value, "NZ=0; NG=");
EndFunction

Procedure VisitExprList(ExprList, Buffer)
	If ExprList.Count() > 0 Then
		For Each Expr In ExprList Do
			If Expr = Undefined Then
				Buffer.Add("");
			Else
				VisitExpr(Expr, Buffer);
			EndIf;
			Buffer.Add(", ");
		EndDo;
		Buffer[Buffer.UBound()] = "";
	EndIf;
EndProcedure // VisitExprList()

Procedure VisitTail(Tail, Buffer)
	For Each Item In Tail Do
		If Item.Type = Nodes.FieldExpr Then
			Buffer.Add(".");
			Buffer.Add(Item.Name);
			If Item.Args <> Undefined Then
				Buffer.Add("(");
				VisitExprList(Item.Args, Buffer);
				Buffer.Add(")");
			EndIf;
		ElsIf Item.Type = Nodes.IndexExpr Then
			Buffer.Add("[");
			VisitExpr(Item.Expr, Buffer);
			Buffer.Add("]");
		Else
			Raise "Unknown selector kind";
		EndIf;
	EndDo;  
EndProcedure // VisitTail()

Function VisitTailRef(Tail, Buffer)
	For Index = 0 To Tail.UBound()-1 Do
		Item = Tail[Index];
		If Item.Type = Nodes.FieldExpr Then
			Buffer.Add(".");
			Buffer.Add(Item.Name);
			If Item.Args <> Undefined Then
				Buffer.Add("(");
				VisitExprList(Item.Args, Buffer);
				Buffer.Add(")");
			EndIf;
		ElsIf Item.Type = Nodes.IndexExpr Then
			Buffer.Add("[");
			VisitExpr(Item.Expr, Buffer);
			Buffer.Add("]");
		Else
			Raise "Unknown selector kind";
		EndIf;
	EndDo;
	Item = Tail[Tail.UBound()];
	If Item.Type = Nodes.FieldExpr Then
		If Item.Args = Undefined Then
			Return StrTemplate("""%1""", Item.Name);
		Else
			Buffer.Add(".");
			Buffer.Add(Item.Name);
			Buffer.Add("(");
			VisitExprList(Item.Args, Buffer);
			Buffer.Add(")");
		EndIf;
	ElsIf Item.Type = Nodes.IndexExpr Then
		IndexBuffer = New Array;
		VisitExpr(Item.Expr, IndexBuffer);
		Return StrConcat(IndexBuffer);
	Else
		Raise "Unknown selector kind";
	EndIf;
	Return Undefined;
EndFunction // VisitTailRef()

#EndRegion // Aux
