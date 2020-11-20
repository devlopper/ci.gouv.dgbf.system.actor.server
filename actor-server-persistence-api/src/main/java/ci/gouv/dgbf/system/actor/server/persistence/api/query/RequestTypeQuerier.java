package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;

public interface RequestTypeQuerier extends Querier {

	RequestType readOne(QueryExecutorArguments arguments);
	Collection<RequestType> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	/* read order by code ascending */
	String QUERY_IDENTIFIER_READ_ALL = QueryIdentifierBuilder.getInstance().build(RequestType.class, "readAllOrderByNameAscending");
	Collection<RequestType> readAll();
	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION = "readByIdentifierForRequestCreation";
	RequestType readByIdentifierForRequestCreation(String identifier);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestTypeQuerier,Serializable {
		
		@Override
		public RequestType readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION))
				return readByIdentifierForRequestCreation((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestType> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_ALL))
				return readAll();
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestType> readAll() {
			return QueryExecutor.getInstance().executeReadMany(RequestType.class, QUERY_IDENTIFIER_READ_ALL);
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
			,Query.buildSelect(RequestType.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_REQUEST_CREATION
					, "SELECT t FROM RequestType t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
		);
	}
}