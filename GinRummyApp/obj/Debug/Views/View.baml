   M S B A M L   `   `   `  ����c  _System.Windows.Interactivity, Version=4.5.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35qPclr-namespace:System.Windows.Interactivity;assembly=System.Windows.InteractivitySystem.Windows.Interactivity  5   	   C ?GinRummy, Version=1.0.0.0, Culture=neutral, PublicKeyToken=nullclr-namespace:QUTQUT 5   	   � 5   	   \ XPresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35R NWindowsBase, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35W SPresentationCore, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35D 9http://schemas.microsoft.com/winfx/2006/xaml/presentation    R NSystem.Xaml, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089:x,http://schemas.microsoft.com/winfx/2006/xaml     5   	   P LPrism.Wpf, Version=6.3.0.0, Culture=neutral, PublicKeyToken=40ee6c3a2184dc59$prismhttp://prismlibrary.com/  5   	   XiPclr-namespace:System.Windows.Interactivity;assembly=System.Windows.Interactivity  5   	   localclr-namespace:QUT  5   	     � Title$  	Gin Rummy��5   	   $	��800��6   $
��1200��6(    � 	Resources 5        QUT.ViewModel   	ViewModel QUT.HiddenCardConverter  HiddenCardConverter QUT.VisibleCardConverter  VisibleCardConverter QUT.DeckPanel  	DeckPanel��%t   &	        &	      &	 &     &	 9        5   
   6     5	   
   6$     5
   
   6%     5   
   6   5       � DataContext 5      �� 5   
    �� ResourceKey$ 	ViewModel��6   .��5      � ��5   
   �� 5       �� ImageSource$ ../Resources/green.jpg��6    j� TileMode$
 Tile=�6>    j� ViewportUnits$ Absolute=�6O    j� Viewport$ 0,0,507,338�6h    � RowDefinitions 5   
   �� 5      $��50 �6   �� 5      $��* �6   �� 5      $��* �6   �� 5      $��* �6   �� 5      $��50 �6   	 � ColumnDefinitions	 5   
   �� 5      $��* �6   .   (System.Windows.Interactivity.Interaction
   Triggers
 5    
   F  @Prism.Interactivity.InteractionRequest.InteractionRequestTrigger  5!      3   -System.Windows.Interactivity.EventTriggerBase   SourceObject 6.   �� *NotificationRequest+ �� Mode$ OneWay=�.   (System.Windows.Interactivity.TriggerBase   Actions. 5"      +  %Prism.Interactivity.PopupWindowAction  !   CenterOverAssociatedObject .    IsModal . 6L   5#      .+�5&   
   ��  �� Orientation$ 
Horizontal=�6   $��Center=�6.   $��0��6L   5'      �� 5)   
   $��1��6B   ��6   ��  �� Path$ ComputerCards���5*      �� 5+      .R�5,      �� $��80��6   $��5q�6'   ��5-      �� 5.       �� 	Converter# [ 6&   50      51      ��53      �� 54      .R�55      �� $ 
Horizontal=�6!   56      58      �� 5:   
   $ 
Horizontal=�6   $��Center=�6.   $��2��6L   .+�5<      �� $	��100��6V   $��20(�6b   $	��100��6p   $��10,10,50,10q�6}   ��6   �� *ButtonCommand+��67   �� *
ButtonName+6}   �� 5=      $��Continue��68   $	��100��6K   $��20(�6W   $	��100��6e   $��10,10,50,10q�6r   ��6   �� *ContinueButton+6r   �� 5?      ��6   �� $ Discards���5@      �� 5A      .R�5B      �� $��80��6    ��5C      �� 5D   "   # [ 6*   
 5F      /	   )System.Windows.Interactivity.EventTrigger	  5G   "    	  	EventName$ 	MouseDown��61   . 5H   &   6
   0System.Windows.Interactivity.InvokeCommandAction
   
  Command 6<   ��  �� Source# [  $$ TakeCardFromDiscardPileCommand� 
  CommandParameter 6�   �� 5I   #   5K      5L      ��5N      �� 5O      .R�5P        5Q      5S      �� 5U      ��6   �� $ RemainingDeck���5V      �� 5W      .R�5X      �� $��80��6    ��5Y      �� 5Z   "   # [ 6*   
 5\      	  5]   "   $ 	MouseDown��61   . 5^   &   
   6<   �� # [  $ TakeCardFromDeckCommand� 6�   �� 5_   #   5a      5b      ��5d      �� 5e      .R�5f        5g      5i      5j      �� 5m   
   $��3��6@   ��6   �� $ 
HumanCards���5n      �� 5o      .R�5p      �� $��80��6   $��5q�6'   ��5q      �� 5r      # [ 6&   
 5t      	  5u      $ 	MouseDown��6-   . 5v   "   
   68   �� # [  $  DiscardCardFromHandCommand� 6�   �� 5w      5y      5z      ��5|      �� 5}      .R�5~      �� $ 
Horizontal=�6!   5      5�      �� 5�   
   $ 
Horizontal=�6   $��Center=�6.   $��4��6L   .+�5�      �� 
�������$��20(�6'   $��Center=�65   ��6R   �� *HumanDeadwood+�� 5�      
�������$��20(�6'   $��Center=�65   ��6R   �� *Score+5�      5�      5�      