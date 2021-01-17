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

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;

public interface AdministrativeUnitQuerier extends Querier.CodableAndNamable<AdministrativeUnit> {

	String PARAMETER_NAME_SECTIONS_IDENTIFIERS = "sectionsIdentifiers";
	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionIdentifier");
	Collection<AdministrativeUnit> readBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionIdentifierForUI");
	Collection<AdministrativeUnit> readBySectionIdentifierForUI(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER);
	Long countBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionsIdentifiers");
	Collection<AdministrativeUnit> readBySectionsIdentifiers(Collection<String> sectionsIdentifiers);
	
	String QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI = QueryIdentifierBuilder.getInstance().build(AdministrativeUnit.class, "readBySectionsIdentifiersForUI");
	Collection<AdministrativeUnit> readBySectionsIdentifiersForUI(Collection<String> sectionsIdentifiers);
	
	String QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS);
	Long countBySectionsIdentifiers(Collection<String> sectionsIdentifiers);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<AdministrativeUnit> implements AdministrativeUnitQuerier,Serializable {

		@SuppressWarnings("unchecked")
		@Override
		public Collection<AdministrativeUnit> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER))
				return readBySectionIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI))
				return readBySectionIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS))
				return readBySectionsIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI))
				return readBySectionsIdentifiersForUI((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			return super.readMany(arguments);
			//throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@SuppressWarnings("unchecked")
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS))
				return countBySectionsIdentifiers((Collection<String>) arguments.getFilterFieldValue(PARAMETER_NAME_SECTIONS_IDENTIFIERS));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER))
				return countBySectionIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			return super.count(arguments);
			//throw new RuntimeException(arguments.getQuery().getIdentifier()+" cannot be processed");
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER
					,sectionIdentifier);
		}
		
		@Override
		public Long countBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionIdentifierForUI(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionsIdentifiers(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS, PARAMETER_NAME_SECTIONS_IDENTIFIERS
					,sectionsIdentifiers);
		}
		
		@Override
		public Long countBySectionsIdentifiers(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS, PARAMETER_NAME_SECTIONS_IDENTIFIERS,sectionsIdentifiers);
		}
		
		@Override
		public Collection<AdministrativeUnit> readBySectionsIdentifiersForUI(Collection<String> sectionsIdentifiers) {
			return QueryExecutor.getInstance().executeReadMany(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI, PARAMETER_NAME_SECTIONS_IDENTIFIERS
					,sectionsIdentifiers);
		}
		
		@Override
		protected Class<AdministrativeUnit> getKlass() {
			return AdministrativeUnit.class;
		}
	}
	
	/**/
	
	static AdministrativeUnitQuerier getInstance() {
		return Helper.getInstance(AdministrativeUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(AdministrativeUnit.class);
		QueryHelper.addQueries(
			Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER
					, "SELECT t FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" ORDER BY t.code ASC")
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER
					, "SELECT COUNT(t.identifier) FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER)
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AdministrativeUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME)
				
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS
					, "SELECT t FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS+" ORDER BY t.code ASC")
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_COUNT_BY_SECTIONS_IDENTIFIERS
					, "SELECT COUNT(t.identifier) FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS)
			
			,Query.buildSelect(AdministrativeUnit.class, QUERY_IDENTIFIER_READ_BY_SECTIONS_IDENTIFIERS_FOR_UI
					, "SELECT t.identifier,t.code,t.name FROM AdministrativeUnit t WHERE t.section.identifier IN :"+PARAMETER_NAME_SECTIONS_IDENTIFIERS+" ORDER BY t.code ASC")
				.setTupleFieldsNamesIndexesFromFieldsNames(AdministrativeUnit.FIELD_IDENTIFIER,AdministrativeUnit.FIELD_CODE,AdministrativeUnit.FIELD_NAME)	
		);
	}
}