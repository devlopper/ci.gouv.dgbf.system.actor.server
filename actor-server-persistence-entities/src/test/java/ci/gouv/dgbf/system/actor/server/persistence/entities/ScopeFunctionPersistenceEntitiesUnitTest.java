package ci.gouv.dgbf.system.actor.server.persistence.entities;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import org.junit.jupiter.api.Test;

public class ScopeFunctionPersistenceEntitiesUnitTest extends AbstractUnitTestMemory {
	private static final long serialVersionUID = 1L;

	@Test
	public void scopeFunction_getOrderNumberFromCode(){
		assertThat(ScopeFunction.getOrderNumberFromCode(null)).isNull();
		assertThat(ScopeFunction.getOrderNumberFromCode("")).isNull();
		assertThat(ScopeFunction.getOrderNumberFromCode("A10076")).isNull();
		assertThat(ScopeFunction.getOrderNumberFromCode("A1007611")).isNull();
		assertThat(ScopeFunction.getOrderNumberFromCode("A100761")).isEqualTo(761);
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
	public void scopeFunction_computeCodeOnly(){
		String nameScript = "poste.libelle == null ? poste.fonction.libelle+' '+poste.ua.libelle : poste.libelle";
		ScopeFunction scopeFunction = new ScopeFunction()
				.setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"))
				.setLocality(new Locality().setCode(Locality.CODE_SOUS_PREFECTURE_BINGERVILLE).setName("S/P Bingerville"));
			
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		scopeFunction.setName("");
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		scopeFunction.setName(" ");
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		scopeFunction.setName("This is my name");
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("This is my name");
	}
	
	@Test
	public void scopeFunction_computeCode_creditManager(){
		String nameScript = "poste.libelle == null ? poste.fonction.libelle+' '+poste.ua.libelle : poste.libelle";
		ScopeFunction scopeFunction = new ScopeFunction()
				.setScope(new Scope().setCode("13010222").setName("DTI").setType(new ScopeType().setCode("UA")))
				.setFunction(new Function().setCode(Function.CODE_CREDIT_MANAGER_HOLDER).setName("Gestionnaire de crédits"));
			
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits DTI");
		
		scopeFunction.setName("Gestionnaire de crédits de carburant de la DTI");
		ScopeFunction.computeCodeAndName("UA", List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("GC13010222");
		assertThat(scopeFunction.getName()).isEqualTo("Gestionnaire de crédits de carburant de la DTI");
	}
	
	//@Test
	public void scopeFunction_computeCode_authorizingOfficer(){
		String nameScript = "poste.libelle == null ? poste.fonction.libelle+' '+poste.ua.libelle : poste.libelle";
		ScopeFunction scopeFunction = new ScopeFunction()
				.setScope(new Scope().setCode("22086").setName("Budget").setType(new ScopeType().setCode(ScopeType.CODE_USB)))
				.setFunction(new Function().setCode(Function.CODE_AUTHORIZING_OFFICER_HOLDER).setName("Ordonnateur"))
				.setLocality(new Locality().setCode("l01").setName("Dimbokro"));
			
		ScopeFunction.computeCodeAndName(ScopeType.CODE_USB, List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("O300000");
		assertThat(scopeFunction.getName()).isEqualTo("Ordonnateur Budget Dimbokro");
		
		scopeFunction.setName("Ordonnateur Pgr. Budget à Dimbokro");
		ScopeFunction.computeCodeAndName(ScopeType.CODE_USB, List.of(scopeFunction),0,0, "poste.fonction.code+poste.ua.code", nameScript);
		assertThat(scopeFunction.getCode()).isEqualTo("O300000");
		assertThat(scopeFunction.getName()).isEqualTo("Ordonnateur Pgr. Budget à Dimbokro");
	}
}