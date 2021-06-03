package ci.gouv.dgbf.system.actor.server.persistence.impl.unit;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.Date;

import javax.persistence.PersistenceException;

import org.cyk.utility.persistence.EntityManagerGetter;
import org.cyk.utility.persistence.query.EntityReader;
import org.cyk.utility.persistence.query.QueryExecutorArguments;
import org.cyk.utility.persistence.server.MetricsManager;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.AdministrativeUnitQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.AssignmentsQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.LocalityQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.api.query.ScopeFunctionQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Locality;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

public class PersistenceImplUnitTestDev extends AbstractUnitTestLive {
	private static final long serialVersionUID = 1L;
	
	@Override
	protected String getPersistenceUnitName() {
		return "dev";
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxOrderNumberByFunctionCode_GC(){
		assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode("GC", 8526);
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G1(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G1", "G108526");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G2(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G2", "G200751");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_G3(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("G3", "G301190");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxOrderNumberByFunctionCode_OD(){
		assert_scopeFunctionQuerier_readMaxOrderNumberByFunctionCode("ORD", 2515);
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_O2(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("O2", "O202515");
	}
	
	@Test
	public void scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith_O3(){
		assert_scopeFunctionQuerier_readMaxCodeWhereCodeStartsWith("O3", "O302514");
	}
	
	@Test
	public void scopeFunctionQuerier_readByIdentifierForUI(){
		QueryExecutorArguments queryExecutorArguments = new QueryExecutorArguments();
		queryExecutorArguments.setQueryFromIdentifier(ScopeFunctionQuerier.QUERY_IDENTIFIER_READ_BY_IDENTIFIER_FOR_UI);
		queryExecutorArguments.addFilterFieldsValues(ScopeFunctionQuerier.PARAMETER_NAME_IDENTIFIER,"G100761");
		queryExecutorArguments.addProcessableTransientFieldsNames(ScopeFunction.FIELD_ACTORS_CODES);
		ScopeFunction scopeFunction = ScopeFunctionQuerier.getInstance().readOne(queryExecutorArguments);
		assertThat(scopeFunction.getIdentifier()).isNotBlank();
		assertThat(scopeFunction.getActorsCodes()).containsExactly("yay.diomande@budget.gouv.ci");
	}
	
	@Test
	public void localities_regions(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION));
		assertThat(regions).isNotNull();
	}

	@Test
	public void localities_regions_typeAsString(){
		Collection<Locality> regions = EntityReader.getInstance().readManyDynamically(Locality.class, new QueryExecutorArguments()
				.addFilterFieldsValues(LocalityQuerier.PARAMETER_NAME_TYPE,Locality.Type.REGION.name()));
		assertThat(regions).isNotNull();
	}
	
	@Test
	public void administrativeUnitQuerier_readByIdentifierWithCodesNamesForUI(){
		AdministrativeUnit administrativeUnit = AdministrativeUnitQuerier.getInstance().readByIdentifierWithCodesNamesForUI("UAba28d795-8bec-4550-afa1-b76e8f8e8de6");
		assertThat(administrativeUnit).isNotNull();
		assertThat(administrativeUnit.getSection()).isNotNull();
		assertThat(administrativeUnit.getSubPrefecture()).isNotNull();
		assertThat(administrativeUnit.getDepartment()).isNotNull();
		assertThat(administrativeUnit.getRegion()).isNotNull();
		
		assertThat(administrativeUnit.getCode()).isEqualTo("26080841");
		assertThat(administrativeUnit.getName()).isEqualTo("Ecoles Primaires Publiques (EPP) Tanda");
		
		assertThat(administrativeUnit.getSection().getCode()).isEqualTo("331");
		assertThat(administrativeUnit.getSection().getName()).isEqualTo("Ministère de l'Education Nationale et de l'Alphabétisation");
		
		assertThat(administrativeUnit.getSubPrefecture().getCode()).isEqualTo("620501");
		assertThat(administrativeUnit.getSubPrefecture().getName()).isEqualTo("Sous-préfecture de Tanda ");
		
		assertThat(administrativeUnit.getDepartment().getCode()).isEqualTo("6205");
		assertThat(administrativeUnit.getDepartment().getName()).isEqualTo("DEPARTEMENT DE TANDA");
		
		assertThat(administrativeUnit.getRegion().getCode()).isEqualTo("62");
		assertThat(administrativeUnit.getRegion().getName()).isEqualTo("REGION DU GONTOUGO");
	}
	
	@Test
	public void assignments_export_entityManagerNotProvided() {
		MetricsManager.getInstance().enable();
		for(Integer index = 0; index < 100; index = index + 1) {
			AssignmentsQuerier.getInstance().export("test", "test", "test", new Date(), null);
		}
		MetricsManager.getInstance().disable();
	}
	
	@Test
	public void assignments_export_entityManagerProvided() {
		Assertions.assertThrows(PersistenceException.class, () -> {
			for(Integer index = 0; index < 100; index = index + 1) {
				AssignmentsQuerier.getInstance().export("test", "test", "test", new Date(), EntityManagerGetter.getInstance().get());
			}
		});
	}
}