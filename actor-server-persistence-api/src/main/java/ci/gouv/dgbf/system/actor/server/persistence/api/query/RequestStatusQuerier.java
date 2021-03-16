package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import static org.cyk.utility.persistence.query.Language.jpql;
import static org.cyk.utility.persistence.query.Language.From.from;
import static org.cyk.utility.persistence.query.Language.Select.select;
import static org.cyk.utility.persistence.query.Language.Where.where;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;

public interface RequestStatusQuerier extends Querier {

	RequestStatus readOne(QueryExecutorArguments arguments);
	Collection<RequestStatus> readMany(QueryExecutorArguments arguments);
	Long count(QueryExecutorArguments arguments);
	
	String QUERY_IDENTIFIER_READ_BY_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(RequestStatus.class, "readByCodeForUI");
	RequestStatus readByCodeForUI(String code);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.AbstractImpl implements RequestStatusQuerier,Serializable {
		
		@Override
		public RequestStatus readOne(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_CODE_FOR_UI))
				return readByCodeForUI((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<RequestStatus> readMany(QueryExecutorArguments arguments) {
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public RequestStatus readByCodeForUI(String code) {
			RequestStatus status = QueryExecutor.getInstance().executeReadOne(RequestStatus.class,new QueryExecutorArguments().setQueryFromIdentifier(
					QUERY_IDENTIFIER_READ_BY_CODE_FOR_UI).addFilterFieldsValues(PARAMETER_NAME_CODE,code));
			return status;
		}
	}
	
	/**/
	
	static RequestTypeQuerier getInstance() {
		return Helper.getInstance(RequestTypeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		QueryManager.getInstance().register(
			Query.buildSelect(RequestStatus.class, QUERY_IDENTIFIER_READ_BY_CODE_FOR_UI, jpql(select("t"),from("RequestStatus t"),where("t.code = :"+PARAMETER_NAME_CODE)))
		);
	}
}