package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;

import org.apache.commons.lang3.StringUtils;
import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;
import org.cyk.utility.persistence.query.Querier;
import org.cyk.utility.persistence.query.Query;
import org.cyk.utility.persistence.query.QueryExecutor;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.persistence.query.QueryManager;
import org.cyk.utility.persistence.query.QueryName;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutor;
import org.cyk.utility.persistence.server.query.executor.DynamicOneExecutor;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;

public interface LocalityQuerier extends Querier.CodableAndNamable<Locality> {

	String QUERY_IDENTIFIER_READ_BY_CODE = QueryIdentifierBuilder.getInstance().build(Locality.class, "readByCode");
	Locality readByCode(String code);
	
	String QUERY_IDENTIFIER_READ_SUB_PREFECTURE_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI = QueryIdentifierBuilder.getInstance().build(Locality.class, "readSubPrefectureByIdentifierWithCodesNamesForUI");
	String QUERY_IDENTIFIER_READ_DEPARTMENT_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI = QueryIdentifierBuilder.getInstance().build(Locality.class, "readDepartmentByIdentifierWithCodesNamesForUI");	
	Locality readByIdentifierWithCodesNamesForUI(String identifier,Locality.Type type);
	
	String QUERY_IDENTIFIER_READ_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Locality.class, QueryName.READ_DYNAMIC);	
	String QUERY_IDENTIFIER_READ_DYNAMIC_ONE = QueryIdentifierBuilder.getInstance().build(Locality.class, QueryName.READ_DYNAMIC_ONE);
	String QUERY_IDENTIFIER_COUNT_DYNAMIC = QueryIdentifierBuilder.getInstance().build(Locality.class, QueryName.COUNT_DYNAMIC);
	
	String PARAMETER_NAME_TYPE = "type";
	String PARAMETER_NAME_PARENT_IDENTIFIER = "parentIdentifier";
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<Locality> implements LocalityQuerier,Serializable {
		
		@Override
		public Locality readByCode(String code) {
			return QueryExecutor.getInstance().executeReadOne(Locality.class, new QueryExecutorArguments().setQueryFromIdentifier(QUERY_IDENTIFIER_READ_BY_CODE)
					.addFilterFieldsValues(PARAMETER_NAME_CODE,code));
		}
		
		@Override
		public Locality readOne(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_SUB_PREFECTURE_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByIdentifierWithCodesNamesForUI((String) arguments.getFilterFieldValue(LocalityQuerier.PARAMETER_NAME_IDENTIFIER),Locality.Type.SOUS_PREFECTURE);
			if(QUERY_IDENTIFIER_READ_DEPARTMENT_BY_IDENTIFIER_WITH_CODES_NAMES_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readByIdentifierWithCodesNamesForUI((String) arguments.getFilterFieldValue(LocalityQuerier.PARAMETER_NAME_IDENTIFIER),Locality.Type.DEPARTEMENT);
			if(QUERY_IDENTIFIER_READ_DYNAMIC_ONE.equals(arguments.getQuery().getIdentifier()))
				return DynamicOneExecutor.getInstance().read(Locality.class,arguments.setQuery(null));
			return super.readOne(arguments);
		}
		
		@Override
		public Collection<Locality> readMany(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_READ_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().read(Locality.class,arguments.setQuery(null));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(QUERY_IDENTIFIER_COUNT_DYNAMIC.equals(arguments.getQuery().getIdentifier()))
				return DynamicManyExecutor.getInstance().count(Locality.class,arguments.setQuery(null));
			return super.count(arguments);
		}
		
		@Override
		public Locality readByIdentifierWithCodesNamesForUI(String identifier,Locality.Type type) {
			Locality locality = DynamicOneExecutor.getInstance().read(Locality.class, new QueryExecutorArguments().addProjectionsFromStrings(Locality.FIELD_IDENTIFIER
					,Locality.FIELD_CODE,Locality.FIELD_NAME,Locality.FIELD_REGION_IDENTIFIER,Locality.FIELD_REGION_CODE_NAME
					,Locality.FIELD_DEPARTMENT_IDENTIFIER,Locality.FIELD_DEPARTMENT_CODE_NAME).addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_IDENTIFIER,identifier));
			if(StringHelper.isNotBlank(locality.getRegionIdentifier()))
				locality.setRegion(new Locality().setIdentifier(locality.getRegionIdentifier()).setCode(StringUtils.substringBefore(locality.getRegionCodeName(), " "))
					.setName(StringUtils.substringAfter(locality.getRegionCodeName(), " ")));
			if(StringHelper.isNotBlank(locality.getDepartmentIdentifier()))
				locality.setDepartment(new Locality().setIdentifier(locality.getDepartmentIdentifier()).setCode(StringUtils.substringBefore(locality.getDepartmentCodeName(), " "))
					.setName(StringUtils.substringAfter(locality.getDepartmentCodeName(), " ")));
			return locality;
		}
		
		@Override
		protected Class<Locality> getKlass() {
			return Locality.class;
		}
	}
	
	/**/
	
	static LocalityQuerier getInstance() {
		return Helper.getInstance(LocalityQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(Locality.class);
		QueryManager.getInstance().register(
				Query.buildSelect(Locality.class, QUERY_IDENTIFIER_READ_BY_CODE, "SELECT t FROM Locality t WHERE t.code = :"+PARAMETER_NAME_CODE)
			);
	}
}