import React from 'react';
import logo from './logo.svg';
import './App.css';
import {gql} from "apollo-boost";
import ApolloClient from "apollo-client";
import {ApolloProvider, Subscription} from 'react-apollo';
import { split } from 'apollo-link';
import { HttpLink } from 'apollo-link-http';
import { WebSocketLink } from 'apollo-link-ws';
import { getMainDefinition } from 'apollo-utilities';
import { InMemoryCache } from 'apollo-cache-inmemory';
import { onError } from 'apollo-link-error';
import { ApolloLink } from 'apollo-link';


const httpLink = new HttpLink({
  uri: 'http://localhost:8080/graphql'
});

const wsLink = new WebSocketLink({
//  uri: `ws://localhost:2345/graphqlSubscribe`,
  uri: `ws://localhost:8080/graphqlSubscribe`,
  options: {
    reconnect: false
  }
});


// using the ability to split links, you can send data to each link
// depending on what kind of operation is being sent
const link = split(
  // split based on operation type
  ({ query }) => {
    const definition = getMainDefinition(query);
    return (
      definition.kind === 'OperationDefinition' &&
      definition.operation === 'subscription'
    );
  },
  wsLink,
  httpLink,
);
/*
const client = new ApolloClient({
  link: link
});
*/

const client = new ApolloClient({
  link: ApolloLink.from([
    onError(({ graphQLErrors, networkError }) => {
      if (graphQLErrors)
        graphQLErrors.map(({ message, locations, path }) =>
          console.log(
            `[GraphQL error]: Message: ${message}, Location: ${locations}, Path: ${path}`,
          ),
        );
      if (networkError) console.log(`[Network error]: ${networkError}`);
    }),
    link
  ]),
  cache: new InMemoryCache()
});

const COMMENTS_SUBSCRIPTION = gql`
  subscription onCommentAdded($repoFullName: String!) {
    rates(repoFullName: $repoFullName) {
      currency
    }
  }
`;

const DontReadTheComments = ({ repoFullName }) => (
  <Subscription
    subscription={COMMENTS_SUBSCRIPTION}
    variables={{ repoFullName }}
  >
    {(data) => {
      console.log(JSON.stringify(data))
      return (
        <h4>New comment: </h4>
      )
    }}
  </Subscription>
);
/*
client
  .query({
    query: COMMENTS_SUBSCRIPTION
  })
  .then(result => console.log(result));
  */

function App() {
  return (
    <ApolloProvider client={client}>
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
    <DontReadTheComments repoFullName="test" />
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
    </ApolloProvider>
  );
}

export default App;
