using Prism.Commands;
using Prism.Interactivity.InteractionRequest;
using System;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Threading.Tasks;
using System.Windows.Input;

namespace QUT
{
    class ViewModel: INotifyPropertyChanged
    {
        public ObservableCollection<Cards.Card> HumanCards { get; private set; }
        public ObservableCollection<Cards.Card> ComputerCards { get; private set; }
        public ObservableCollection<Cards.Card> Discards { get; private set; }
        public ObservableCollection<Cards.Card> RemainingDeck { get; private set; }

        public InteractionRequest<INotification> NotificationRequest { get; private set; }

        public ICommand ButtonCommand { get; set; }
        public ICommand ContinueButton { get; set; }
        public ICommand DiscardCardFromHandCommand { get; set; }
        public ICommand TakeCardFromDiscardPileCommand { get; set; }
        public ICommand TakeCardFromDeckCommand { get; set; }
        public int humanScore { get; set; }
        public int compScore { get; set; }
        bool computerTurn = false;
        bool humanTurn = true;

        public event PropertyChangedEventHandler PropertyChanged;

        public ViewModel()
        {
            TakeCardFromDiscardPileCommand = new DelegateCommand<Cards.Card>(TakeCardFromDiscardPile);
            DiscardCardFromHandCommand = new DelegateCommand<Cards.Card>(DiscardCardFromHand);
            TakeCardFromDeckCommand = new DelegateCommand<Cards.Card>(TakeCardFromDeck);

            ButtonCommand = new DelegateCommand(ButtonClick);
            ContinueButton = new DelegateCommand(Continue);

            NotificationRequest = new InteractionRequest<INotification>();

            HumanCards = new ObservableCollection<Cards.Card>();
            ComputerCards = new ObservableCollection<Cards.Card>();
            Discards = new ObservableCollection<Cards.Card>();
            RemainingDeck = new ObservableCollection<Cards.Card>();

            HumanCards.CollectionChanged += HumanCards_CollectionChanged;

            humanScore = 0;
            compScore = 0;

            ButtonName = "Gin/Knock";


            Deal();
            
        }



        private async void Deal()
        {
            var deck = Cards.Shuffle(Cards.FullDeck);

            foreach (var card in deck)
            {
                RemainingDeck.Add(card);
                await Task.Delay(1);
            }

            for (int i = 0; i < 10; i++)
            {
                ComputerCards.Add(DrawTopCardFromDeck());
                await Task.Delay(30);
                HumanCards.Add(DrawTopCardFromDeck());
                await Task.Delay(30);
            }

            Discards.Add(DrawTopCardFromDeck());
        }

        private Cards.Card DrawTopCardFromDeck()
        {
            var top = RemainingDeck[RemainingDeck.Count - 1];
            RemainingDeck.Remove(top);
            return top;
        }

        private void TakeCardFromDeck(Cards.Card card)
        {
            if(humanTurn)
            {
                RemainingDeck.Remove(card);
                HumanCards.Add(card);
            }
        }

        private void TakeCardFromDiscardPile(Cards.Card p)
        {
            if(humanTurn)
            {
                Discards.Remove(p);
                HumanCards.Add(p);
            }
        }

        private void DiscardCardFromHand(Cards.Card p)
        {
            if(HumanCards.Count > 10 && humanTurn)
            {
                HumanCards.Remove(p);
                Discards.Add(p);
                humanTurn = false;
            }
        }

        async private void HumanCards_CollectionChanged(object sender, System.Collections.Specialized.NotifyCollectionChangedEventArgs e)
        {
            HumanDeadwood = "Calculating ...";
            Score = "Human Score: " + humanScore + " - Computer Score: " + compScore;
            // this might take a while, so let's do it in the background
            int deadwood = await Task.Run(() => GinRummy.Deadwood(HumanCards));
            HumanDeadwood = "Deadwood: " + deadwood;
            if(deadwood == 0)
            {
                ButtonName = "GIN";
            }
            else
            {
                ButtonName = "Knock";
            }

            if(humanScore > 99 || compScore > 99)
            {
                humanScore = 0;
                compScore = 0;
                CleanUpCards();
            }

        }

        private void ComputerTurn()
        {
            var takeDiscard = ComputerPlayer.ComputerPickupDiscard(ComputerCards, Discards[Discards.Count - 1], RemainingDeck);

            if (takeDiscard)
            {
                var discardCard = Discards[Discards.Count - 1];
                Discards.Remove(discardCard);
                ComputerCards.Add(discardCard);
            }
            else
            {
                Cards.Card remainingDeckCards = RemainingDeck[RemainingDeck.Count - 1];
                RemainingDeck.Remove(remainingDeckCards);
                ComputerCards.Add(remainingDeckCards);
            }

            var compMove = ComputerPlayer.ComputerMove(ComputerCards);

            ComputerCards.Remove(compMove.Item2.Value);
            Discards.Add(compMove.Item2.Value);

            var scores = GinRummy.Score(ComputerCards, HumanCards);

            if (compMove.Item1.IsGin)
            {
                compScore += scores;
                if(humanScore < 100 && compScore < 100)
                {
                    RaiseNotification("Human Score: " + humanScore + ". Computer Score: " + compScore, "Scores So Far");
                    CleanUpCards();
                }             
            }
            else if(compMove.Item1.IsKnock)
            {
                if(scores > 0)
                {
                    compScore += scores;
                    if (humanScore < 100 && compScore < 100)
                    {
                        RaiseNotification("Human Score: " + humanScore + ". Computer Score: " + compScore, "Scores So Far");
                        CleanUpCards();
                    }
                }
                else
                {
                    humanScore += Math.Abs(scores);
                    if (humanScore < 100 && compScore < 100)
                    {
                        RaiseNotification("Human Score: " + humanScore + ". Computer Score: " + compScore, "Scores So Far");
                        CleanUpCards();
                    }
                }
            }
        }

        private void CleanUpCards()
        {
            HumanCards.Clear();
            ComputerCards.Clear();
            RemainingDeck.Clear();
            Discards.Clear();
            Deal();
        }


        private string buttonName;

        public string ButtonName
        {
            get { return buttonName; }

            set
            {
                buttonName = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("ButtonName"));
            }
        }

        private string score;

        public string Score
        {
            get { return score; }
            set {
                score = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("Score"));
            }
        }


        private string humanDeadwood;

        public string HumanDeadwood 
        { 
            get
            {
                return humanDeadwood;
            }
            private set
            {
                humanDeadwood = value;
                if (PropertyChanged != null)
                    PropertyChanged(this, new PropertyChangedEventArgs("HumanDeadwood"));
            }
        }

        private void RaiseNotification(string msg, string title)
        {
            NotificationRequest.Raise(new Notification { Content = msg, Title = title });
        }

        private async void ButtonClick()
        {
            var scores = GinRummy.Score(HumanCards, ComputerCards);
            if(scores > 0)
            {
                humanScore += scores;
            }
            else
            {
                compScore += Math.Abs(scores);
            }
            RaiseNotification("Human Score: " + humanScore + ". Computer Score: " + compScore, "Scores So Far");
            CleanUpCards();

        }

        private void Continue()
        {
            humanTurn = false;
            ComputerTurn();
            humanTurn = true;
        }
    }
}