package ci.gouv.dgbf.system.actor.server.persistence.api;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

import org.cyk.utility.__kernel__.array.ArrayHelper;
import org.cyk.utility.__kernel__.security.SecurityHelper;
import org.cyk.utility.persistence.query.EntityCreator;
import org.junit.jupiter.api.Test;

import ci.gouv.dgbf.system.actor.server.persistence.api.query.PrivilegeQuerier;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Privilege;
import ci.gouv.dgbf.system.actor.server.persistence.entities.PrivilegeType;

public class PersistenceApiPrivilegeUnitTest extends AbstractUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected void initializeEntityManagerFactory(String persistenceUnitName) {
		super.initializeEntityManagerFactory(persistenceUnitName);
		ApplicationScopeLifeCycleListener.initialize();
		SecurityHelper.PRINCIPALABLE.set(Boolean.FALSE);
	}
	
	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void privielgeQuerier_readParentsByChildrenCodes_1(){
		assertReadParentsByChildrenCodes(List.of("1"));
	}
	
	@Test
	public void privielgeQuerier_readParentsByChildrenCodes_2(){
		assertReadParentsByChildrenCodes(List.of("2"),"1");
	}
	
	@Test
	public void privielgeQuerier_readParentsByChildrenCodes_3(){
		assertReadParentsByChildrenCodes(List.of("3"),"1","2");
	}
	
	@Test
	public void privielgeQuerier_readParentsByChildrenCodes_4(){
		assertReadParentsByChildrenCodes(List.of("4"),"1","2","3");
	}
	
	@Test
	public void privielgeQuerier_readParentsByChildrenCodes_5(){
		assertReadParentsByChildrenCodes(List.of("5"),"1","2","3","4");
	}
	
	@Override
	protected void createData() {
		EntityCreator.getInstance().createManyInTransaction(new PrivilegeType().setCode("1").setName("1"));
		EntityCreator.getInstance().createManyInTransaction(
				new Privilege().setCode("1").setName("1").setTypeFromIdentifier("1")
				,new Privilege().setCode("2").setName("2").setTypeFromIdentifier("1").setParentIdentifier("1")
				,new Privilege().setCode("3").setName("3").setTypeFromIdentifier("1").setParentIdentifier("2")
				,new Privilege().setCode("4").setName("4").setTypeFromIdentifier("1").setParentIdentifier("3")
				,new Privilege().setCode("5").setName("5").setTypeFromIdentifier("1").setParentIdentifier("4")
			);
	}

	private void assertReadParentsByChildrenCodes(Collection<String> childrenCodes,String...expectedPrivilegesCodes) {
		Collection<Privilege> privileges = PrivilegeQuerier.getInstance().readParentsByChildrenCodes(childrenCodes);
		if(ArrayHelper.isEmpty(expectedPrivilegesCodes))
			assertThat(privileges).isNull();
		else
			assertThat(privileges.stream().map(privilege -> privilege.getCode()).collect(Collectors.toList())).containsExactly(expectedPrivilegesCodes);
	}
}