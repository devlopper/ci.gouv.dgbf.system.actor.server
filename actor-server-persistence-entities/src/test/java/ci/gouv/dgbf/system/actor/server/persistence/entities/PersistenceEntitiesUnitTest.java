package ci.gouv.dgbf.system.actor.server.persistence.entities;

import static org.assertj.core.api.Assertions.assertThat;
import java.util.Collection;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

public class PersistenceEntitiesUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void scopeFunction_setCodeFromScript(){
		String codeScript = "code_fonction+code_ua";
		String nameScript = "libelle_fonction+' '+libelle_ua";
		ScopeFunction scopeFunction = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode("GC").setName("Gestionnaire de crédits"));
		assertThat(scopeFunction.getCode()).isNull();
		assertThat(scopeFunction.setCodeFromScript(codeScript).getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.setNameFromScript(nameScript).getName()).isEqualTo("Gestionnaire de crédits DTI");
	}
	
	@Test
	public void buildFieldNames(){
		assertThat(ExecutionImputation.FIELD_CREDIT_MANAGER_HOLDER_SCOPE_FUNCTION_IDENTIFIER).isEqualTo("creditManagerHolderScopeFunctionIdentifier");
	}
	
	@Test
	public void privilege_getParent(){
		Collection<Privilege> privileges = CollectionHelper.listOf(
				new Privilege().setIdentifier("M1")
				,new Privilege().setIdentifier("S1").setParentIdentifier("M1")
				,new Privilege().setIdentifier("S2").setParentIdentifier("M1")
				,new Privilege().setIdentifier("ME1").setParentIdentifier("S1")
				,new Privilege().setIdentifier("ME1.1").setParentIdentifier("ME1")
				,new Privilege().setIdentifier("ME1.2").setParentIdentifier("ME1")
				,new Privilege().setIdentifier("ME2").setParentIdentifier("S1")
				,new Privilege().setIdentifier("ME2.1").setParentIdentifier("ME2")
				,new Privilege().setIdentifier("ME2.2").setParentIdentifier("ME2")
				,new Privilege().setIdentifier("ME2.3").setParentIdentifier("ME2")
			);
		assertParent(privileges, "M1",null);
		assertParent(privileges, "S1","M1");
		assertParent(privileges, "ME1","S1");
		assertParent(privileges, "ME1.1","ME1");
	}
	
	@Test
	public void privilege_isLeaf(){
		Collection<Privilege> privileges = CollectionHelper.listOf(
				new Privilege().setIdentifier("M1")
				,new Privilege().setIdentifier("S1").setParentIdentifier("M1")
				,new Privilege().setIdentifier("S2").setParentIdentifier("M1")
				,new Privilege().setIdentifier("ME1").setParentIdentifier("S1")
				,new Privilege().setIdentifier("ME1.1").setParentIdentifier("ME1")
				,new Privilege().setIdentifier("ME1.2").setParentIdentifier("ME1")
				,new Privilege().setIdentifier("ME2").setParentIdentifier("S1")
				,new Privilege().setIdentifier("ME2.1").setParentIdentifier("ME2")
				,new Privilege().setIdentifier("ME2.2").setParentIdentifier("ME2")
				,new Privilege().setIdentifier("ME2.3").setParentIdentifier("ME2")
			);
		assertIsLeaf(privileges, "M1",Boolean.FALSE);
		assertIsLeaf(privileges, "S1",Boolean.FALSE);
		assertIsLeaf(privileges, "ME1",Boolean.FALSE);
		assertIsLeaf(privileges, "ME1.1",Boolean.TRUE);
	}
	
	private static void assertParent(Collection<Privilege> privileges,String childIdentifier,String expectedParentIdentifier) {
		Privilege parent = Privilege.getParent(privileges, getPrivilegeByIdentifier(privileges,childIdentifier));
		if(parent == null)
			assertThat(parent).isNull();
		else
			assertThat(parent.getIdentifier()).isEqualTo(expectedParentIdentifier);
	}
	
	private static void assertIsLeaf(Collection<Privilege> privileges,String identifier,Boolean expected) {
		assertThat(Privilege.isLeaf(privileges, getPrivilegeByIdentifier(privileges, identifier))).isEqualTo(expected);
	}
	
	private static Privilege getPrivilegeByIdentifier(Collection<Privilege> privileges,String identifier) {
		if(CollectionHelper.isEmpty(privileges))
			return null;
		for(Privilege privilege : privileges)
			if(privilege.getIdentifier().equals(identifier))
				return privilege;
		return null;
	}
}