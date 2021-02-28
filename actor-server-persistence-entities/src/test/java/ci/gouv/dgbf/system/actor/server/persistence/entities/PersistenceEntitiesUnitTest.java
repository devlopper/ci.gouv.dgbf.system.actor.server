package ci.gouv.dgbf.system.actor.server.persistence.entities;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.test.weld.AbstractPersistenceUnitTest;
import org.junit.jupiter.api.Test;

public class PersistenceEntitiesUnitTest extends AbstractPersistenceUnitTest {
	private static final long serialVersionUID = 1L;

	@Override
	protected String getPersistenceUnitName() {
		return "default";
	}
	
	@Test
	public void IdentificationAttribut_CODES_FIELDS_NAMES(){
		System.out.println(IdentificationAttribute.CODES_FIELDS_NAMES);
	}
	
	@Test
	public void scopeFunction_scopeType_computeCodeAndName(){
		ScopeFunction scopeFunction0 = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"))
				.setLocality(new Locality().setCode(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE).setName("S/P Bingerville"));
		ScopeFunction scopeFunction1 = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"))
				.setLocality(new Locality().setCode("deconcentre").setName("Bouake"));
		ScopeFunction scopeFunction2 = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"))
				.setLocality(new Locality().setCode("deconcentre").setName("Man"));
		
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction0,scopeFunction1,scopeFunction2),0,0, "poste.fonction.code+poste.ua.code", "poste.fonction.libelle+' '+poste.ua.libelle");
		assertThat(scopeFunction0.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction0.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction0,scopeFunction1,scopeFunction2),0,0, "poste.fonction.code+poste.ua.code+(poste.localite.code == '"+Locality.CODE_SOUS_PREFECTURE_BINGERVILLE
				+"' ? '0' : ++numero_ordre)", "poste.fonction.libelle+' '+poste.ua.libelle");
		assertThat(scopeFunction0.getCode()).isEqualTo("GC130102220");
		assertThat(scopeFunction0.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction1.getCode()).isEqualTo("GC130102221");
		assertThat(scopeFunction1.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction2.getCode()).isEqualTo("GC130102222");
		assertThat(scopeFunction2.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction1,scopeFunction0,scopeFunction2),0,0, "poste.fonction.code+poste.ua.code+(poste.localite.code == '"+Locality.CODE_SOUS_PREFECTURE_BINGERVILLE
				+"' ? '0' : ++numero_ordre)", "poste.fonction.libelle+' '+poste.ua.libelle");
		assertThat(scopeFunction0.getCode()).isEqualTo("GC130102220");
		assertThat(scopeFunction0.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction1.getCode()).isEqualTo("GC130102221");
		assertThat(scopeFunction1.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction2.getCode()).isEqualTo("GC130102222");
		assertThat(scopeFunction2.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction1,scopeFunction0,scopeFunction2),5,0, "poste.fonction.code+poste.ua.code+(poste.localite.code == '"+Locality.CODE_SOUS_PREFECTURE_BINGERVILLE
				+"' ? '0' : ++numero_ordre)", "poste.fonction.libelle+' '+poste.ua.libelle");
		assertThat(scopeFunction0.getCode()).isEqualTo("GC130102220");
		assertThat(scopeFunction0.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction1.getCode()).isEqualTo("GC130102226");
		assertThat(scopeFunction1.getName()).isEqualTo("Gestionnaire de crédits DTI");
		assertThat(scopeFunction2.getCode()).isEqualTo("GC130102227");
		assertThat(scopeFunction2.getName()).isEqualTo("Gestionnaire de crédits DTI");
	}
	
	@Test
	public void scopeFunction_scopeType_computeCodeAndName_assistants(){
		ScopeFunction parent = new ScopeFunction().setCode("G100000");
		Scope scope = new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA"));
		Function function = new Function().setCode(Function.CODE_CREDIT_MANAGER_ASSISTANT).setName("Gestionnaire de crédits");
		
		///String code = "('A1'+('00000'+(numero_ordre++)).slice(-5))+(numero_ordre_2++)";
		String code = "('A1'+(poste.titulaire == null || poste.titulaire.code == null ? '' : poste.titulaire.code).slice(-5))+(poste.titulaire.incrementerNombreAssistant()-1)";
		String name = "poste.fonction.libelle+' '+poste.ua.libelle";
		
		ScopeFunction assistant0 = new ScopeFunction().setScope(scope).setFunction(function).setParent(parent);
		ScopeFunction assistant1 = new ScopeFunction().setScope(scope).setFunction(function).setParent(parent);
		ScopeFunction assistant2 = new ScopeFunction().setScope(scope).setFunction(function).setParent(parent);
		
		ScopeFunction.computeCodeAndName("UA", List.of(assistant0),0,0, code, name);
		assertThat(assistant0.getCode()).isEqualTo("A1000000");
		assertThat(assistant0.getName()).isEqualTo("Gestionnaire de crédits DTI");
		ScopeFunction.computeCodeAndName("UA", List.of(assistant1),0,1, code, name);
		assertThat(assistant1.getCode()).isEqualTo("A1000001");
		assertThat(assistant1.getName()).isEqualTo("Gestionnaire de crédits DTI");
		ScopeFunction.computeCodeAndName("UA", List.of(assistant2),0,2, code, name);
		assertThat(assistant2.getCode()).isEqualTo("A1000002");
		assertThat(assistant2.getName()).isEqualTo("Gestionnaire de crédits DTI");
	}
	
	@Test
	public void scopeFunction_usb_locality_computeCodeAndName(){
		ScopeFunction scopeFunction = new ScopeFunction().setCode("MyCode").setName("TheName")
				.setScope(new Scope().setCode("22086").setName("Budget").setType(new ScopeType().setCode("USB")))
				.setFunction(new Function().setCode("OS").setName("Ordonnateur secondaire"))
				;
		
		ScopeFunction.computeCodeAndName("USB", List.of(scopeFunction),0,0, "'O3'+code_usb+numero_ordre", "libelle_fonction+' '+libelle_usb+' '+libelle_localite");
		assertThat(scopeFunction.getCode()).isEqualTo("O3220861");
		assertThat(scopeFunction.getName()).isEqualTo("Ordonnateur secondaire Budget bouna");
	}
	
	@Test
	public void scopeFunction_setCodeFromScript(){
		String codeScript = "code_fonction+code_ua";
		String nameScript = "libelle_fonction+' '+libelle_ua";
		ScopeFunction scopeFunction = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"));
		assertThat(scopeFunction.getCode()).isNull();
		assertThat(scopeFunction.setCodeFromScript(codeScript).getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.setNameFromScript(nameScript).getName()).isEqualTo("Gestionnaire de crédits DTI");
	}
	
	@Test
	public void scopeFunctions_computeCodeAndName(){
		String codeScript = "code_fonction+code_ua";
		String nameScript = "libelle_fonction+' '+libelle_ua";
		ScopeFunction scopeFunction = new ScopeFunction().setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"));
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, codeScript, nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits DTI");
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