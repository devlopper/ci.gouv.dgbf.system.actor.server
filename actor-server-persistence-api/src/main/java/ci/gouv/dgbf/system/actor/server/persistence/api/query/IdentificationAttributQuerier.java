package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.QueryName;
import org.cyk.utility.__kernel__.persistence.query.TypedQuerier;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribut;

public interface IdentificationAttributQuerier extends TypedQuerier<IdentificationAttribut,String> {

	String QUERY_IDENTIFIER_READ_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationAttribut.class, QueryName.READ_FOR_UI.getValue());	
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationAttribut.class, QueryName.READ_BY_IDENTIFIER_FOR_UI.getValue());
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(IdentificationAttribut.class, QueryName.READ_BY_IDENTIFIER_FOR_EDIT.getValue());
	
	/**/
	
	public static abstract class AbstractImpl extends TypedQuerier.AbstractImpl<IdentificationAttribut,String> implements IdentificationAttributQuerier,Serializable {
		
		@Override
		protected Class<IdentificationAttribut> getResultClass() {
			return IdentificationAttribut.class;
		}
		
		@Override
		protected IdentificationAttribut __readOne__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.__readOne__(arguments);
		}
		
		@Override
		protected Collection<IdentificationAttribut> __readMany__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_FOR_UI))
				return readForUI(arguments);
			return super.__readMany__(arguments);
		}
		
		@Override
		protected Long __count__(QueryExecutorArguments arguments) {
			return super.__count__(arguments);
		}
	}
	
	/**/
	
	static IdentificationAttributQuerier getInstance() {
		return Helper.getInstance(IdentificationAttributQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		TypedQuerier.initialize(IdentificationAttribut.class);
		QueryHelper.addQueries(
			Query.buildSelect(IdentificationAttribut.class, QUERY_IDENTIFIER_READ_FOR_UI, "SELECT t.identifier,t.code,t.name FROM IdentificationAttribut t ORDER BY t.code ASC")
			.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationAttribut.FIELD_IDENTIFIER,IdentificationAttribut.FIELD_CODE,IdentificationAttribut.FIELD_NAME)
			,Query.buildSelect(IdentificationAttribut.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
					, "SELECT t FROM IdentificationAttribut t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			,Query.buildSelect(IdentificationAttribut.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
					, "SELECT t FROM IdentificationAttribut t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
		);
	}
}