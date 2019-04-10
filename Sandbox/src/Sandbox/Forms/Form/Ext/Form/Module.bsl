
&AtClient
Procedure Run(Command)
	
	If OnServer Then
		RunAtServer();
	Else
		RunAtClient();
	EndIf; 
	
EndProcedure 

&AtClient
Procedure RunAtClient()
	
	Var SP, FP, M, IP, ES, EP, RP, ErrorInfo;
	
	Code = CompileAtServer();
	Compiled.SetText(StrConcat(Code, Chars.LF));
	
	IP = 0; SP = 0; FP = 0; M = New Map; ES = New Map; EP = 0; RP = 0;
	
	Start = CurrentUniversalDateInMilliseconds();
	
	While IP < Code.Count() Do
		
		Text = Code[IP];
		
		Try
			Execute Text;
			IP = IP + 1;
		Except
			
			If EP > 0 Then
				
				// очистка стека от мусора
				For P = ES[EP]+1 To SP Do
					M[P] = Undefined;
				EndDo;
				ES[EP] = Undefined;
				EP = EP - 1;
				
				// переход к блоку Except
				IP = ES[EP];
				ES[EP] = Undefined;
				EP = EP - 1;
				
				Text = Code[IP];
				
				ErrorInfo = ErrorInfo();
				
				Execute Text;
				IP = IP + 1;
				
			Else	
				Raise;
			EndIf;
			
		EndTry;
		
	EndDo;
	
	Message("time: " + (CurrentUniversalDateInMilliseconds() - Start) / 1000);
	
EndProcedure

&AtServer
Procedure RunAtServer()
	
	Var SP, FP, M, IP, ES, EP, RP, ErrorInfo;
	
	Code = CompileAtServer();
	Compiled.SetText(StrConcat(Code, Chars.LF));
	
	IP = 0; SP = 0; FP = 0; M = New Map; ES = New Map; EP = 0; RP = 0;
	
	Start = CurrentUniversalDateInMilliseconds();
	
	While IP < Code.Count() Do
		
		Text = Code[IP];
		
		Try
			Execute Text;
			IP = IP + 1;
		Except
			
			If EP > 0 Then
				
				// очистка стека от мусора
				For P = ES[EP]+1 To SP Do
					M[P] = Undefined;
				EndDo;
				ES[EP] = Undefined;
				EP = EP - 1;
				
				// переход к блоку Except
				IP = ES[EP];
				ES[EP] = Undefined;
				EP = EP - 1;
				
				Text = Code[IP];
				
				ErrorInfo = ErrorInfo();
				
				Execute Text;
				IP = IP + 1;
				
			Else	
				Raise;
			EndIf;
			
		EndTry;
		
	EndDo;
	
	Message("time: " + (CurrentUniversalDateInMilliseconds() - Start) / 1000);
	
EndProcedure

&AtServer
Function CompileAtServer()
	
	This = FormAttributeToValue("Object");
	ThisFile = New File(This.UsedFileName);

	BSLParser = ExternalDataProcessors.Create(ThisFile.Path + "BSLParser.epf", False);
	Compiler = ExternalDataProcessors.Create(ThisFile.Path + "Compiler.epf", False);
	
	BSLParser.Go(Source.GetText(), Compiler);
	
	Return Compiler.Result();
	
EndFunction 
