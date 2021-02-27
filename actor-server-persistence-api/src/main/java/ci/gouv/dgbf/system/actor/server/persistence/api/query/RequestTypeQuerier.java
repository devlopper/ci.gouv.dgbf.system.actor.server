package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Select.kaseBooleanYesNo;
import static org.cyk.utility.persistence.query.Language.Select.fields;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Where.where;
import static org.cyk.utility.persistence.query.Language.Order.order;
import static org.cyk.utility.persistence.query.Language.Order.asc;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryHelper;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public interface RequestTypeQuerier extends Querier {

	RequestType readOne(QueryExecutorArguments arguments);
	Collection<RequestType> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read order by code ascending */
	String QUERY_IDENTIFIER_READ_ALL = QueryIdentifierBuilder.getInstance().build(RequestType.class, "readAll");
	Collection<RequestType> readAll();
	
	String QUERY_IDENTIFIER_READ_FOR_UI = QueryIdentifierBuilder.getInstance().build(RequestType.class, "readForUI");
	Collection<RequestType> readForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_COUNT_FOR_UI = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_FOR_UI);
	Long countForUI(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = "readByIdentifierForEdit";
	RequestType readByIdentifierForEdit(String identifier);
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION = "readByIdentifierForRequestCreation";
	RequestType readByIdentifierForRequestCreation(String identifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestTypeQuerier,Serializable {
		
		@Override
		public RequestType readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION))
				return readByIdentifierForRequestCreation((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestType> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_ALL))
				return readAll();
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_FOR_UI))
				return readForUI(arguments);
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_FOR_UI))
				return countForUI(arguments);
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestType> readAll() {
			return QueryExecutor.getInstance().executeReadMany(RequestType.class, QUERY_IDENTIFIER_READ_ALL);
		}
		
		@Override
		public Collection<RequestType> readForUI(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_READ_FOR_UI);
			return QueryExecutor.getInstance().executeReadMany(RequestType.class, arguments);
		}
		
		@Override
		public Long countForUI(QueryExecutorArguments arguments) {
			if(arguments.getQuery() == null)
				arguments.setQueryFromIdentifier(QUERY_IDENTIFIER_COUNT_FOR_UI);
			return QueryExecutor.getInstance().executeCount(arguments);
		}
		
		@Override
		public RequestType readByIdentifierForEdit(String identifier) {
			RequestType type = QueryExecutor.getInstance().executeReadOne(RequestType.class,new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			return type;
		}
		
		@Override
		public RequestType readByIdentifierForRequestCreation(String identifier) {
			RequestType type = QueryExecutor.getInstance().executeReadOne(RequestType.class,new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION).addFilterFieldsValues(PARAMETER_NAME_IDENTIFIER,identifier));
			if(type.getForm() != null)
				IdentificationFormQuerier.AbstractImpl.setFields(type.getForm(), null);
			return type;
		}
	}
	
	/**/
	
	static RequestTypeQuerier getInstance() {
		return Helper.getInstance(RequestTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryHelper.addQueries(
			Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_READ_ALL, "SELECT t FROM RequestType t ORDER BY t.code ASC")
			,Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_READ_FOR_UI, 
					jpql(
							select(fields("t",RequestType.FIELD_IDENTIFIER,RequestType.FIELD_CODE,RequestType.FIELD_NAME,"form.name")
									,kaseBooleanYesNo("t", RequestType.FIELD_AUTHENTICATION_REQUIRED,"'Non'"))
							,from("RequestType t")
							,order(asc("t","code"))))
			.setTupleFieldsNamesIndexesFromFieldsNames(RequestType.FIELD_IDENTIFIER,RequestType.FIELD_CODE,RequestType.FIELD_NAME,RequestType.FIELD_FORM_AS_STRING
					,RequestType.FIELD_AUTHENTICATION_REQUIRED_AS_STRING)
			,Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_COUNT_FOR_UI, "SELECT COUNT(t.identifier) FROM RequestType t")
			,Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
					, jpql(select("t"),from("RequestType t"),where("t.identifier = :"+PARAMETER_NAME_IDENTIFIER)))
			,Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION
					, "SELECT t FROM RequestType t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
		);
	}
}