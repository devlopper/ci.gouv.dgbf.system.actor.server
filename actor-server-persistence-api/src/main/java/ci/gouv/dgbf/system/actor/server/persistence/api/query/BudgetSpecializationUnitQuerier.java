package ci.gouv.dgbf.system.actor.server.persistence.api.query;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.Helper;
import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.field.FieldHelper;
import org.cyk.utility.__kernel__.persistence.query.Querier;
import org.cyk.utility.__kernel__.persistence.query.Query;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutor;
import org.cyk.utility.__kernel__.persistence.query.QueryExecutorArguments;
import org.cyk.utility.__kernel__.persistence.query.QueryHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryIdentifierBuilder;
import org.cyk.utility.__kernel__.persistence.query.annotation.Queries;
import org.cyk.utility.__kernel__.string.StringHelper;
import org.cyk.utility.__kernel__.value.Value;

import ci.gouv.dgbf.system.actor.server.persistence.api.BudgetSpecializationUnitPersistence;
import ci.gouv.dgbf.system.actor.server.persistence.entities.BudgetSpecializationUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Scope;

@Queries(value = {
		@org.cyk.utility.__kernel__.persistence.query.annotation.Query(tupleClass = BudgetSpecializationUnit.class,name = BudgetSpecializationUnitQuerier.QUERY_NAME_READ
				,value = "SELECT t FROM BudgetSpecializationUnit t ORDER BY t.code ASC")
	})
public interface BudgetSpecializationUnitQuerier extends Querier.CodableAndNamable<BudgetSpecializationUnit> {

	String PARAMETER_NAME_SECTION_IDENTIFIER = "sectionIdentifier";
	
	/* read order by code ascending */
	String QUERY_NAME_READ = "read";
	String QUERY_IDENTIFIER_READ = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, QUERY_NAME_READ);
	Collection<BudgetSpecializationUnit> read();
	
	String QUERY_IDENTIFIER_COUNT = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ);
	Long count();
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, "readBySectionIdentifier");
	Collection<BudgetSpecializationUnit> readBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, "readBySectionIdentifierForUI");
	Collection<BudgetSpecializationUnit> readBySectionIdentifierForUI(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER = QueryIdentifierBuilder.getInstance().buildCountFrom(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER);
	Long countBySectionIdentifier(String sectionIdentifier);
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, "readVisiblesByActorCodeForUI");
	Collection<BudgetSpecializationUnit> readVisiblesByActorCodeForUI(String actorCode);
	
	String QUERY_IDENTIFIER_READ_VISIBLES_BY_SECTION_IDENTIFIER_BY_ACTOR_CODE_FOR_UI = QueryIdentifierBuilder.getInstance().build(BudgetSpecializationUnit.class, "readVisiblesBySectionIdentifierByActorCodeForUI");
	Collection<BudgetSpecializationUnit> readVisiblesBySectionIdentifierByActorCodeForUI(String sectionIdentifier,String actorCode);
	
	/**/
	
	public static abstract class AbstractImpl extends Querier.CodableAndNamable.AbstractImpl<BudgetSpecializationUnit> implements BudgetSpecializationUnitQuerier,Serializable {
		
		@Override
		public Collection<BudgetSpecializationUnit> read() {
			return QueryExecutor.getInstance().executeReadMany(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ);
		}
		
		@Override
		public Long count() {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT);
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readMany(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER))
				return readBySectionIdentifier((String)arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI))
				return readBySectionIdentifierForUI((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(QUERY_IDENTIFIER_READ.equals(arguments.getQuery().getIdentifier()))
				return read();
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			if(QUERY_IDENTIFIER_READ_VISIBLES_BY_SECTION_IDENTIFIER_BY_ACTOR_CODE_FOR_UI.equals(arguments.getQuery().getIdentifier()))
				return readVisiblesBySectionIdentifierByActorCodeForUI((String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_SECTION_IDENTIFIER)
						,(String) arguments.getFilterFieldValue(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE));
			return super.readMany(arguments);
		}
		
		@Override
		public Long count(QueryExecutorArguments arguments) {
			if(arguments.getQuery().getIdentifier().equals(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER))
				return countBySectionIdentifier((String) arguments.getFilterFieldValue(PARAMETER_NAME_SECTION_IDENTIFIER));
			if(QUERY_IDENTIFIER_COUNT.equals(arguments.getQuery().getIdentifier()))
				return count();
			return super.count(arguments);
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER
					,sectionIdentifier);
		}
		
		@Override
		public Long countBySectionIdentifier(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeCount(QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readBySectionIdentifierForUI(String sectionIdentifier) {
			return QueryExecutor.getInstance().executeReadMany(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
					, PARAMETER_NAME_SECTION_IDENTIFIER,sectionIdentifier);
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readVisiblesBySectionIdentifierByActorCodeForUI(String sectionIdentifier,String actorCode) {
			if(StringHelper.isBlank(actorCode))
				return null;
			Collection<Scope> scopes =  ScopeOfTypeBudgetSpecializationUnitQuerier.getInstance().readVisibleWhereFilter(new QueryExecutorArguments()
					.setQueryFromIdentifier(ScopeOfTypeBudgetSpecializationUnitQuerier.QUERY_IDENTIFIER_READ_VISIBLE_WHERE_FILTER)
					.addFilterFieldsValues(ScopeQuerier.PARAMETER_NAME_ACTOR_CODE, actorCode));
			if(CollectionHelper.isEmpty(scopes))
				return null;
			Collection<BudgetSpecializationUnit> budgetSpecializationUnits = __inject__(BudgetSpecializationUnitPersistence.class)
					.readBySystemIdentifiers(FieldHelper.readSystemIdentifiers(scopes));
			if(StringHelper.isNotBlank(sectionIdentifier))
				budgetSpecializationUnits = budgetSpecializationUnits.stream().filter(x -> x.getSection() != null && x.getSection().getIdentifier().equals(sectionIdentifier))
				.collect(Collectors.toList());
			return budgetSpecializationUnits.stream().map(x -> new BudgetSpecializationUnit().setIdentifier(x.getIdentifier()).setCode(x.getCode()).setName(x.getName())
					.setSectionIdentifier(x.getSection() == null ? null : x.getSection().getIdentifier()))
					.collect(Collectors.toList());
		}
		
		@Override
		public Collection<BudgetSpecializationUnit> readVisiblesByActorCodeForUI(String actorCode) {
			if(StringHelper.isBlank(actorCode))
				return null;
			return readVisiblesBySectionIdentifierByActorCodeForUI(null,actorCode);
		}
		
		@Override
		protected Class<BudgetSpecializationUnit> getKlass() {
			return BudgetSpecializationUnit.class;
		}
		
	}
	
	/**/
	
	static BudgetSpecializationUnitQuerier getInstance() {
		return Helper.getInstance(BudgetSpecializationUnitQuerier.class, INSTANCE);
	}
	
	Value INSTANCE = new Value();
	
	static void initialize() {
		Querier.CodableAndNamable.initialize(BudgetSpecializationUnit.class);
		QueryHelper.addQueries(
				Query.buildSelect(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER
						, "SELECT t FROM BudgetSpecializationUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER+" ORDER BY t.code ASC")
				
				,Query.buildSelect(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_COUNT_BY_SECTION_IDENTIFIER
						, "SELECT COUNT(t.identifier) FROM BudgetSpecializationUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER)
				
				,Query.buildSelect(BudgetSpecializationUnit.class, QUERY_IDENTIFIER_READ_BY_SECTION_IDENTIFIER_FOR_UI
						, "SELECT t.identifier,t.code,t.name FROM BudgetSpecializationUnit t WHERE t.section.identifier = :"+PARAMETER_NAME_SECTION_IDENTIFIER
						+" ORDER BY t.code ASC")
					.setTupleFieldsNamesIndexesFromFieldsNames(BudgetSpecializationUnit.FIELD_IDENTIFIER,BudgetSpecializationUnit.FIELD_CODE,BudgetSpecializationUnit.FIELD_NAME)
			);
	}
}