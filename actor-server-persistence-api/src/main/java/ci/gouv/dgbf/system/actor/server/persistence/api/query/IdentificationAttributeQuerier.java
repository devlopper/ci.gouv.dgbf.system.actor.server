package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.constant.ConstantEmpty;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.query.TypedQuerier;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationAttribute;

public interface IdentificationAttributeQuerier extends TypedQuerier<IdentificationAttribute,String> {

	String PARAMETER_NAME_FORM_IDENTIFIER = "formIdentifier";
	
	String QUERY_IDENTIFIER_READ_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationAttribute.class, QueryName.READ_FOR_UI.getValue());	
	
	String QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_DO_NOT_EXIST_BY_FORM_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance()
			.build(IdentificationAttribute.class, "readWhereAssociationDoNotExistByFormIdentifierForUI");
	Collection<IdentificationAttribute> readWhereAssociationDoNotExistByFormIdentifierForUI(QueryExecutorArguments arguments);
	/*
	String QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_EXIST_BY_FORM_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance()
			.build(IdentificationAttribute.class, "readWhereAssociationExistByFormIdentifierForUI");
	Collection<IdentificationAttribute> readWhereAssociationExistByFormIdentifierForUI(QueryExecutorArguments arguments);
	*/
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(IdentificationAttribute.class, QueryName.READ_BY_IDENTIFIER_FOR_UI.getValue());
	String QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT = QueryIdentifierBuilder.getInstance().build(IdentificationAttribute.class, QueryName.READ_BY_IDENTIFIER_FOR_EDIT.getValue());
	
	/**/
	
	public static abstract class AbstractImpl extends TypedQuerier.AbstractImpl<IdentificationAttribute,String> implements IdentificationAttributeQuerier,Serializable {
		
		@Override
		public Class<IdentificationAttribute> getType() {
			return IdentificationAttribute.class;
		}
		
		@Override
		protected IdentificationAttribute __readOne__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT))
				return readByIdentifierForEdit((String)arguments.getFilterFieldValue(PARAMETER_NAME_IDENTIFIER));
			return super.__readOne__(arguments);
		}
		
		@Override
		protected Collection<IdentificationAttribute> __readMany__(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_FOR_UI))
				return readForUI(arguments);
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_DO_NOT_EXIST_BY_FORM_IDENTIFIER_FOR_UI))
				return readWhereAssociationDoNotExistByFormIdentifierForUI(arguments);
			//if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_EXIST_BY_FORM_IDENTIFIER_FOR_UI))
			//	return readWhereAssociationExistByFormIdentifierForUI(arguments);
			return super.__readMany__(arguments);
		}
		
		@Override
		public Collection<IdentificationAttribute> readWhereAssociationDoNotExistByFormIdentifierForUI(QueryExecutorArguments arguments) {
			return QueryExecutor.getInstance().executeReadMany(getType(), QueryExecutorArguments.setQueryIfNull(arguments, getType()
					, QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_DO_NOT_EXIST_BY_FORM_IDENTIFIER_FOR_UI));
		}
		/*
		@Override
		public Collection<IdentificationAttribute> readWhereAssociationExistByFormIdentifierForUI(QueryExecutorArguments arguments) {
			return QueryExecutor.getInstance().executeReadMany(getType(), QueryExecutorArguments.setQueryIfNull(arguments, getType()
					, QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_EXIST_BY_FORM_IDENTIFIER_FOR_UI));
		}
		*/
		@Override
		protected Long __count__(QueryExecutorArguments arguments) {
			return super.__count__(arguments);
		}
	}
	
	/**/
	
	static IdentificationAttributeQuerier getInstance() {
		return Helper.getInstance(IdentificationAttributeQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		TypedQuerier.initialize(IdentificationAttribute.class);
		QueryManager.getInstance().register(
			Query.buildSelect(IdentificationAttribute.class, QUERY_IDENTIFIER_READ_FOR_UI, "SELECT t.identifier,t.code,t.name FROM IdentificationAttribute t ORDER BY t.code ASC")
			.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationAttribute.FIELD_IDENTIFIER,IdentificationAttribute.FIELD_CODE,IdentificationAttribute.FIELD_NAME)
			
			,buildReadWhereAssociationExist(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_DO_NOT_EXIST_BY_FORM_IDENTIFIER_FOR_UI, Boolean.FALSE)
			//,buildReadWhereAssociationExist(QUERY_IDENTIFIER_READ_WHERE_ASSOCIATION_EXIST_BY_FORM_IDENTIFIER_FOR_UI, Boolean.TRUE)
			
			,Query.buildSelect(IdentificationAttribute.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI
					, "SELECT t FROM IdentificationAttribute t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
			,Query.buildSelect(IdentificationAttribute.class, QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_EDIT
					, "SELECT t FROM IdentificationAttribute t WHERE t.identifier = :"+PARAMETER_NAME_IDENTIFIER)
		);
	}
	
	static Query buildReadWhereAssociationExist(String identifier,Boolean isExist) {
		return Query.buildSelect(IdentificationAttribute.class, identifier
				, "SELECT t.identifier,t.code,t.name "
				+ "FROM IdentificationAttribute t "
				+ "WHERE "+(Boolean.TRUE.equals(isExist) ? ConstantEmpty.STRING : "NOT")+" EXISTS(SELECT c FROM IdentificationFormAttribute c WHERE c.attribute = t AND c.form.identifier = :"+PARAMETER_NAME_FORM_IDENTIFIER+")"
				+ "ORDER BY t.code ASC"
				)
		.setTupleFieldsNamesIndexesFromFieldsNames(IdentificationAttribute.FIELD_IDENTIFIER,IdentificationAttribute.FIELD_CODE,IdentificationAttribute.FIELD_NAME);
	}
}