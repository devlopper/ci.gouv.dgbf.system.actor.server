package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.stream.Collectors;

import org.cyk.utility.persistence.EntityManagerFactoryGetterImpl;
import org.cyk.utility.persistence.EntityManagerGetterImpl;
import org.cyk.utility.persistence.ParameterNameBuilderImpl;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.query.QueryGetterImpl;
import org.cyk.utility.persistence.query.QueryIdentifierBuilderImpl;
import org.cyk.utility.persistence.query.QueryIdentifierGetterImpl;
import org.cyk.utility.persistence.server.QueryManagerImpl;
import org.cyk.utility.persistence.server.query.QueryExecutorImpl;
import org.cyk.utility.persistence.server.query.executor.DynamicManyExecutorImpl;
import org.cyk.utility.persistence.server.query.string.EqualStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.FromStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.LikeStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.LikeStringValueBuilderImpl;
import org.cyk.utility.persistence.server.query.string.OrderStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.SelectStringBuilderImpl;
import org.cyk.utility.persistence.server.query.string.WhereStringBuilderImpl;
import org.jboss.weld.junit5.WeldInitiator;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.EntityReaderImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.QueryResultMapperImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ExpenditureNatureQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActionQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityCategoryQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeActivityQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeAdministrativeUnitQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeBudgetSpecializationUnitQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeOfTypeSectionQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.SectionQuerierImpl;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ExpenditureNature;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;
import ci.gouv.dgbf.system.actor.server.persistence.impl.EntityLifeCycleListenerImpl;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RuntimeQueryBuilderImpl;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.RuntimeQueryStringBuilderImpl;
import ci.gouv.dgbf.system.actor.server.persistence.impl.query.ScopeFunctionQuerierImpl;

public class PersistenceUnitTestSpeed extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected WeldInitiator __getWeldInitiator__() {
		// TODO Auto-generated method stub
		return WeldInitiator.of(QueryManagerImpl.class,QueryIdentifierBuilderImpl.class
				,EntityManagerGetterImpl.class,EntityManagerFactoryGetterImpl.class,QueryIdentifierGetterImpl.class
				,ScopeOfTypeSectionQuerierImpl.class,ScopeOfTypeBudgetSpecializationUnitQuerierImpl.class
				,ScopeOfTypeAdministrativeUnitQuerierImpl.class,ScopeOfTypeActionQuerierImpl.class
				,ScopeOfTypeActivityQuerierImpl.class,ScopeOfTypeActivityCategoryQuerierImpl.class
				,ParameterNameBuilderImpl.class,QueryExecutorImpl.class,QueryGetterImpl.class,RuntimeQueryBuilderImpl.class,RuntimeQueryStringBuilderImpl.class
				,EntityLifeCycleListenerImpl.class,DynamicManyExecutorImpl.class,LikeStringBuilderImpl.class
				,LikeStringValueBuilderImpl.class,EqualStringBuilderImpl.class,QueryStringBuilderImpl.class,SelectStringBuilderImpl.class
				,FromStringBuilderImpl.class,WhereStringBuilderImpl.class,OrderStringBuilderImpl.class,QueryResultMapperImpl.class
				,SectionQuerierImpl.class,ScopeFunctionQuerierImpl.class,ExpenditureNatureQuerierImpl.class,LocalityQuerierImpl.class,EntityReaderImpl.class);
	}
	
	@Test public void t1(){}	
	@Test public void t2(){}
	
	@Test
	public void sectionQuerier_readAll(){
		assertThat(SectionQuerier.getInstance().read().stream().map(Section::getCode)
				.collect(Collectors.toList())).contains("101");
	}
	
	@Test
	public void scopeFunctionQuerier_readAll(){
		assertThat(ScopeFunctionQuerier.getInstance().read().stream().map(ScopeFunction::getCode)
				.collect(Collectors.toList())).contains("A1000000");
	}
	
	@Test
	public void expenditureNature_readAllForUI(){
		assertThat(ExpenditureNatureQuerier.getInstance().readAllForUI().stream().map(ExpenditureNature::getCode)
				.collect(Collectors.toList())).containsExactly("1","2","3","4");
	}
	
	@Test
	public void localities_regions(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION))
				.stream().map(Locality::getCode).collect(Collectors.toList())).contains("r01","r02");
	}
	
	@Test
	public void localities_departmentsByRegionsIdentifiers(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"1"))
				.stream().map(Locality::getCode).collect(Collectors.toList())).isNotNull().contains("d01","d02");
		
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"7"))).isNull();
	}
	
	@Test
	public void localities_sousPrefecturesByDepartmentsIdentifiers(){
		assertThat(EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_PARENT_IDENTIFIER,"2"))
				.stream().map(Locality::getCode).collect(Collectors.toList())).contains("sp01","sp02");		
	}
}