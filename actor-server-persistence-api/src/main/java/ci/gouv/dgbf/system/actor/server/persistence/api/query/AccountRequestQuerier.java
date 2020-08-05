package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.map.MapHelper;
import org.cyk.utility.__kernel__.object.AbstractObject;
import org.cyk.utility.__kernel__.persistence.query.EntityFinder;
import org.cyk.utility.__kernel__.persistence.query.Language;
import org.cyk.utility.__kernel__.persistence.query.Language.From;
import org.cyk.utility.__kernel__.persistence.query.Language.Order;
import org.cyk.utility.__kernel__.persistence.query.Language.Select;
import org.cyk.utility.__kernel__.persistence.query.Language.Where;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.filter.Filter;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Civility;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Identity;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentityGroup;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

public interface AccountRequestQuerier extends Querier {

	String PARAMETER_NAME_FIRST_NAME = "firstName";
	String PARAMETER_NAME_LAST_NAMES = "lastNames";
	String PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS = "electronicMailAddress";
	String PARAMETER_NAME_ACCESS_TOKEN = "accessToken";
	
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME = 2;
	Integer NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES = 4;
	
	public static abstract class AbstractImpl extends AbstractObject implements AccountRequestQuerier,Serializable {
		
		@Override
		public AccountRequest readByElectronicMailAddress(String electronicMailAddress) {
			return QueryExecutor.getInstance().executeReadOne(AccountRequest.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS).addFilterFieldsValues(PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS
					,electronicMailAddress));
		}
		
		@Override
		public AccountRequest readByAccessToken(String accessToken) {
			return QueryExecutor.getInstance().executeReadOne(AccountRequest.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN).addFilterFieldsValues(PARAMETER_NAME_ACCESS_TOKEN
					,accessToken));
		}
		
		@Override
		public AccountRequest readProjection01ByAccessToken(String accessToken) {
			AccountRequest accountRequest = QueryExecutor.getInstance().executeReadOne(AccountRequest.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_PROJECTION_01_BY_ACCESS_TOKEN).addFilterFieldsValues(PARAMETER_NAME_ACCESS_TOKEN
					,accessToken));
			return accountRequest;
		}
		
		@Override
		public AccountRequest readProjection01WithBudgetaryFunctionsAndFunctionsByAccessToken(String accessToken) {
			AccountRequest accountRequest = readProjection01ByAccessToken(accessToken);
			if(accountRequest == null)
				return null;
			accountRequest.setBudgetaryFunctions(BudgetaryFunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			accountRequest.setFunctions(FunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			return accountRequest;
		}
		
		@Override
		public AccountRequest readProjection01ByIdentifier(String identifier) {
			AccountRequest accountRequest = QueryExecutor.getInstance().executeReadOne(AccountRequest.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_PROJECTION_01_BY_IDENTIFIER).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			return accountRequest;
		}
		
		@Override
		public AccountRequest readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier(String identifier) {
			AccountRequest accountRequest = readProjection01ByIdentifier(identifier);
			if(accountRequest == null)
				return null;
			accountRequest.setBudgetaryFunctions(BudgetaryFunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			accountRequest.setFunctions(FunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			return accountRequest;
		}
		
		@Override
		public AccountRequest readProjection02ByIdentifier(String identifier) {
			AccountRequest accountRequest = QueryExecutor.getInstance().executeReadOne(AccountRequest.class, new QueryExecutorArguments()
					.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_PROJECTION_02_BY_IDENTIFIER).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			return accountRequest;
		}
		
		@Override
		public AccountRequest readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(String identifier) {
			AccountRequest accountRequest = readProjection02ByIdentifier(identifier);
			if(accountRequest == null)
				return null;
			Scope scope = EntityFinder.getInstance().find(Scope.class, accountRequest.getAdministrativeUnit().getIdentifier());
			accountRequest.getAdministrativeUnit().setCode(scope.getCode());
			accountRequest.getAdministrativeUnit().setName(scope.getName());
			accountRequest.setBudgetaryFunctions(BudgetaryFunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			accountRequest.setFunctions(FunctionQuerier.getInstance().readByAccountRequestIdentifier(accountRequest.getIdentifier()));
			return accountRequest;
		}
		
		@Override
		public Collection<AccountRequest> readWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeReadMany(AccountRequest.class, arguments);
		}
		
		@Override
		public Long countWhereFilter(QueryExecutorArguments arguments) {
			prepareWhereFilter(arguments);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		private static void prepareWhereFilter(QueryExecutorArguments arguments) {
			Filter filter = new Filter();
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME, arguments);	
			filter.addFieldContainsStringOrWords(PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES, arguments);	
			filter.addFieldContains(PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS, arguments);
			arguments.setFilter(filter);
		}
	}
	
	/**/
	
	/* Read by electronic mail address */
	String QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readByElectronicMailAddress");
	String QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS = Language.of(Select.of("t"),From.ofTuple(AccountRequest.class)
			,Where.of(Where.equals("t.identity", AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS,PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS)));
	AccountRequest readByElectronicMailAddress(String electronicMailAddress);
	
	/* Read by access token */
	String QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readByAccessToken");
	String QUERY_VALUE_READ_BY_ACCESS_TOKEN = Language.of(Select.of("t"),From.ofTuple(AccountRequest.class)
			,Where.of(Where.equals("t", AccountRequest.FIELD_ACCESS_TOKEN,PARAMETER_NAME_ACCESS_TOKEN)));
	AccountRequest readByAccessToken(String accessToken);
	
	/* Read projection 01 by access token */
	String QUERY_IDENTIFIER_READ_PROJECTION_01_BY_ACCESS_TOKEN = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readProjection01ByAccessToken");
	AccountRequest readProjection01ByAccessToken(String accessToken);
	
	/* Read projection 01 with budgetaries functions and funtions by access token */
	String QUERY_IDENTIFIER_READ_PROJECTION_01_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_ACCESS_TOKEN = Querier.buildIdentifier(AccountRequest.class, "readProjection01WithBudgetariesFunctionsAndFunctionsByAccessToken");
	AccountRequest readProjection01WithBudgetaryFunctionsAndFunctionsByAccessToken(String accessToken);
	
	/* Read projection 01 by identifier */
	String QUERY_IDENTIFIER_READ_PROJECTION_01_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readProjection01ByIdentifier");
	AccountRequest readProjection01ByIdentifier(String identifier);
	
	/* Read projection 02 by identifier */
	String QUERY_IDENTIFIER_READ_PROJECTION_02_BY_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readProjection02ByIdentifier");
	AccountRequest readProjection02ByIdentifier(String identifier);
	
	/* Read projection 01 with budgetaries functions and funtions by identifier */
	String QUERY_IDENTIFIER_READ_PROJECTION_01_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_IDENTIFIER = Querier.buildIdentifier(AccountRequest.class, "readProjection01WithBudgetariesFunctionsAndFunctionsByIdentifier");
	AccountRequest readProjection01WithBudgetaryFunctionsAndFunctionsByIdentifier(String identifier);
	
	/* Read projection 02 with budgetaries functions and funtions by identifier */
	String QUERY_IDENTIFIER_READ_PROJECTION_02_WITH_BUDGETARIES_FUNCTIONS_AND_FUNCTIONS_BY_IDENTIFIER = Querier.buildIdentifier(AccountRequest.class, "readProjection02WithBudgetariesFunctionsAndFunctionsByIdentifier");
	AccountRequest readProjection02WithBudgetaryFunctionsAndFunctionsByIdentifier(String identifier);
	
	/* Read where filter */
	String QUERY_IDENTIFIER_READ_WHERE_FILTER = QueryIdentifierBuilder.getInstance().build(AccountRequest.class, "readWhereFilter");
	Map<String,Integer> QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES = MapHelper.instantiateStringIntegerByStrings(AccountRequest.FIELD_IDENTIFIER
			,AccountRequest.FIELD_FIRST_NAME,AccountRequest.FIELD_LAST_NAMES,AccountRequest.FIELD_NAMES,AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS
			,AccountRequest.FIELD_CREATION_DATE);
	String QUERY_VALUE_READ_WHERE_FILTER_WHERE = Where.of(Where.and(
			Where.like("t.identity", AccountRequest.FIELD_FIRST_NAME, PARAMETER_NAME_FIRST_NAME, NUMBER_OF_WORDS_OF_PARAMETER_NAME_FIRST_NAME)
			,Where.like("t.identity", AccountRequest.FIELD_LAST_NAMES, PARAMETER_NAME_LAST_NAMES, NUMBER_OF_WORDS_OF_PARAMETER_NAME_LAST_NAMES)
			,Where.like("t.identity", AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS, PARAMETER_NAME_ELECTRONIC_MAIL_ADDRESS)
			));
	String QUERY_VALUE_READ_WHERE_FILTER = Language.of(Select.of("t.identifier,t.identity.firstName,t.identity.lastNames,"
			+Select.concat("t.identity", AccountRequest.FIELD_FIRST_NAME,AccountRequest.FIELD_LAST_NAMES)+",t.identity."+AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS
			+",t.creationDate")
			,From.ofTuple(AccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE
			,Order.of(Order.join(Order.asc("t.identity", AccountRequest.FIELD_FIRST_NAME),Order.asc("t.identity", AccountRequest.FIELD_LAST_NAMES))));
	Collection<AccountRequest> readWhereFilter(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_WHERE_FILTER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_WHERE_FILTER);
	String QUERY_VALUE_COUNT_WHERE_FILTER = Language.of(Select.of("COUNT(t.identifier)"),From.ofTuple(AccountRequest.class),QUERY_VALUE_READ_WHERE_FILTER_WHERE);
	Long countWhereFilter(QueryExecutorArguments arguments);
	
	/**/
	
	static AccountRequestQuerier getInstance() {
		return Helper.getInstance(AccountRequestQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_WHERE_FILTER
				).setTupleFieldsNamesIndexes(QUERY_VALUE_READ_WHERE_FILTER_TUPLE_FIELDS_NAMES_INDEXES)
			);
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_COUNT_WHERE_FILTER
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,Long.class
				,Query.FIELD_VALUE,QUERY_VALUE_COUNT_WHERE_FILTER
				)
			);
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ACCESS_TOKEN
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ACCESS_TOKEN
				)
			);
		
		QueryHelper.addQueries(buildReadProjection01WithBudgetaryFunctionsAndFunctionsByQuery(QUERY_IDENTIFIER_READ_PROJECTION_01_BY_ACCESS_TOKEN, AccountRequest.FIELD_ACCESS_TOKEN,PARAMETER_NAME_ACCESS_TOKEN));
		QueryHelper.addQueries(buildReadProjection01WithBudgetaryFunctionsAndFunctionsByQuery(QUERY_IDENTIFIER_READ_PROJECTION_01_BY_IDENTIFIER, AccountRequest.FIELD_IDENTIFIER,PARAMETER_NAME_IDENTIFIER));
		QueryHelper.addQueries(buildReadProjection02WithBudgetaryFunctionsAndFunctionsByQuery(QUERY_IDENTIFIER_READ_PROJECTION_02_BY_IDENTIFIER, AccountRequest.FIELD_IDENTIFIER,PARAMETER_NAME_IDENTIFIER));
		
		QueryHelper.addQueries(Query.build(Query.FIELD_IDENTIFIER,QUERY_IDENTIFIER_READ_BY_ELECTRONIC_MAIL_ADDRESS
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,QUERY_VALUE_READ_BY_ELECTRONIC_MAIL_ADDRESS
				)
			);
	}
	
	static Query buildReadProjection01WithBudgetaryFunctionsAndFunctionsByQuery(String identifier,String fieldName,String parameterName) {
		return Query.build(Query.FIELD_IDENTIFIER,identifier
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,Language.of(
						Select.of(
								"t.identifier"
								/* Identity */
								,Select.fields("t.identity",Identity.FIELD_ACT_OF_APPOINTMENT_REFERENCE,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATORY
								,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE,Identity.FIELD_ADMINISTRATIVE_FUNCTION
								,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS,Identity.FIELD_FIRST_NAME
								,Identity.FIELD_LAST_NAMES
								,Identity.FIELD_MOBILE_PHONE_NUMBER,Identity.FIELD_OFFICE_PHONE_EXTENSION,Identity.FIELD_OFFICE_PHONE_NUMBER
								,Identity.FIELD_POSTAL_BOX_ADDRESS,Identity.FIELD_REGISTRATION_NUMBER)
								,Select.fields(Identity.FIELD_CIVILITY, Civility.FIELD_NAME)
								,Select.fields("identityGroup", IdentityGroup.FIELD_NAME)
								,Select.concatCodeName("scope")
								
								/* Request */
								,Select.fields("t",AccountRequest.FIELD_ACCESS_TOKEN,AccountRequest.FIELD_CREATION_DATE,AccountRequest.FIELD_SUBMISSION_DATE)
						)
						,From.ofTuple(AccountRequest.class)
						,"LEFT OUTER JOIN Civility civility ON civility = t.identity.civility"
						,"LEFT OUTER JOIN IdentityGroup identityGroup ON identityGroup = t.identity.group"
						,"LEFT OUTER JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = t.identity.administrativeUnit"
						,"LEFT OUTER JOIN Scope scope ON scope = administrativeUnit"
						,Where.of(Where.equals("t", fieldName,parameterName))
					)).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(AccountRequest.FIELD_IDENTIFIER,AccountRequest.FIELD_ACT_OF_APPOINTMENT_REFERENCE
						,AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATORY,AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_STRING,AccountRequest.FIELD_ADMINISTRATIVE_FUNCTION						
						,AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS,AccountRequest.FIELD_FIRST_NAME						
						,AccountRequest.FIELD_LAST_NAMES,AccountRequest.FIELD_MOBILE_PHONE_NUMBER,AccountRequest.FIELD_OFFICE_PHONE_EXTENSION,AccountRequest.FIELD_OFFICE_PHONE_NUMBER
						,AccountRequest.FIELD_POSTAL_BOX_ADDRESS,AccountRequest.FIELD_REGISTRATION_NUMBER
						,AccountRequest.FIELD_CIVILITY_AS_STRING,AccountRequest.FIELD_GROUP_AS_STRING
						,AccountRequest.FIELD_ADMINISTRATIVE_UNIT_AS_STRING
						,AccountRequest.FIELD_ACCESS_TOKEN,AccountRequest.FIELD_CREATION_DATE_AS_STRING,AccountRequest.FIELD_SUBMISSION_DATE_AS_STRING));
	}
	
	static Query buildReadProjection02WithBudgetaryFunctionsAndFunctionsByQuery(String identifier,String fieldName,String parameterName) {
		return Query.build(Query.FIELD_IDENTIFIER,identifier
				,Query.FIELD_TUPLE_CLASS,AccountRequest.class,Query.FIELD_RESULT_CLASS,AccountRequest.class
				,Query.FIELD_VALUE,Language.of(
						Select.of(
								"t.identifier"
								/* Identity */
								,Select.fields("t.identity",Identity.FIELD_ACT_OF_APPOINTMENT_REFERENCE,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATORY
								,Identity.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE,Identity.FIELD_ADMINISTRATIVE_FUNCTION
								,Identity.FIELD_ELECTRONIC_MAIL_ADDRESS,Identity.FIELD_FIRST_NAME
								,Identity.FIELD_LAST_NAMES
								,Identity.FIELD_MOBILE_PHONE_NUMBER,Identity.FIELD_OFFICE_PHONE_EXTENSION,Identity.FIELD_OFFICE_PHONE_NUMBER
								,Identity.FIELD_POSTAL_BOX_ADDRESS,Identity.FIELD_REGISTRATION_NUMBER,Identity.FIELD_CIVILITY,Identity.FIELD_GROUP,Identity.FIELD_ADMINISTRATIVE_UNIT)							
								/* Request */
								,Select.fields("t",AccountRequest.FIELD_ACCESS_TOKEN,AccountRequest.FIELD_CREATION_DATE,AccountRequest.FIELD_SUBMISSION_DATE)
						)
						,From.ofTuple(AccountRequest.class)
						/*,"LEFT OUTER JOIN Civility civility ON civility = t.identity.civility"
						,"LEFT OUTER JOIN IdentityGroup identityGroup ON identityGroup = t.identity.group"
						,"LEFT OUTER JOIN AdministrativeUnit administrativeUnit ON administrativeUnit = t.identity.administrativeUnit"
						,"LEFT OUTER JOIN Scope scope ON scope = administrativeUnit"
						*/
						,Where.of(Where.equals("t", fieldName,parameterName))
					)).setTupleFieldsNamesIndexes(MapHelper.instantiateStringIntegerByStrings(AccountRequest.FIELD_IDENTIFIER,AccountRequest.FIELD_ACT_OF_APPOINTMENT_REFERENCE
						,AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATORY,AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_TIMESTAMP,AccountRequest.FIELD_ADMINISTRATIVE_FUNCTION						
						,AccountRequest.FIELD_ELECTRONIC_MAIL_ADDRESS,AccountRequest.FIELD_FIRST_NAME						
						,AccountRequest.FIELD_LAST_NAMES,AccountRequest.FIELD_MOBILE_PHONE_NUMBER,AccountRequest.FIELD_OFFICE_PHONE_EXTENSION,AccountRequest.FIELD_OFFICE_PHONE_NUMBER
						,AccountRequest.FIELD_POSTAL_BOX_ADDRESS,AccountRequest.FIELD_REGISTRATION_NUMBER
						,AccountRequest.FIELD_CIVILITY,AccountRequest.FIELD_GROUP
						,AccountRequest.FIELD_ADMINISTRATIVE_UNIT
						,AccountRequest.FIELD_ACCESS_TOKEN,AccountRequest.FIELD_CREATION_DATE_AS_STRING,AccountRequest.FIELD_SUBMISSION_DATE_AS_STRING));
	}
}