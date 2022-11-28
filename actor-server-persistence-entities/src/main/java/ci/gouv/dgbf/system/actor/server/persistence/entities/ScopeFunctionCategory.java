package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunctionCategory.TABLE_NAME)
/* Performance Tuning */
@Cacheable
@org.hibernate.annotations.Cache(usage = org.hibernate.annotations.CacheConcurrencyStrategy.READ_WRITE)
public class ScopeFunctionCategory extends AbstractIdentifiableSystemScalarStringIdentifiableBusinessStringNamableImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@Column(name = COLUMN_BUDGET_CATEGORY_IDENTIFIER)
	private String budgetCategoryIdentifier;
	
	@Override
	public ScopeFunctionCategory setIdentifier(String identifier) {
		return (ScopeFunctionCategory) super.setIdentifier(identifier);
	}
	
	@Override
	public ScopeFunctionCategory setCode(String code) {
		return (ScopeFunctionCategory) super.setCode(code);
	}
	
	@Override
	public ScopeFunctionCategory setName(String name) {
		return (ScopeFunctionCategory) super.setName(name);
	}
	
	public static final String FIELD_BUDGET_CATEGORY_IDENTIFIER = "budgetCategoryIdentifier";
	
	public static final String TABLE_NAME = "CATEGORIE_POSTE";
	
	public static final String COLUMN_BUDGET_CATEGORY_IDENTIFIER = "CATEGORIE_BUDGET";
	
	/**/
	
	public static final String CODE_G1 = "G1";
	public static final String NAME_G1 = "Gestionnaire de crédits";
	
	public static final String CODE_G6 = "G6";
	public static final String NAME_G6 = "Gestionnaire de crédits EPN";
	
	public static final String CODE_O2 = "O2";
	public static final String NAME_O2 = "Ordonnateur délégué";
	
	public static final String CODE_O3 = "O3";
	public static final String NAME_O3 = "Ordonnateur secondaire";
	
	public static final String CODE_O9 = "O9";
	public static final String NAME_O9 = "Autre ordonnateur délégué";
	//public static final String NAME_O9 = "Ordonnateur autres Dépenses Centralisées";
	
	public static final String CODE_C1 = "C1";
	public static final String NAME_C1 = "Contrôleur financier de section";
	
	public static final String CODE_C2 = "C2";
	public static final String NAME_C2 = "Contrôleur financier régional";
	
	public static final String CODE_C3 = "C3";
	public static final String NAME_C3 = "Contrôleur financier de projet";
	
	public static final String CODE_T1 = "T1";
	public static final String NAME_T1 = "Payeur général";
	
	public static final String CODE_T2 = "T2";
	public static final String NAME_T2 = "Payeur régional";
	
	public static final String CODE_T3 = "T3";
	public static final String NAME_T3 = "Payeur à l'étranger";
	
	public static final String CODE_T4 = "T4";
	public static final String NAME_T4 = "Trésorier général";
	
	public static final String CODE_T5 = "T5";
	public static final String NAME_T5 = "Trésorier principal";
	
	public static final String CODE_T6 = "T6";
	public static final String NAME_T6 = "Trésorier régional";
	
	public static final String CODE_T8 = "T8";
	public static final String NAME_T8 = "Trésorier de projet";
	
	public static final String CODE_T9 = "T9";
	public static final String NAME_T9 = "Recetteur général";
	
	public static final String CODE_A1 = "A1";
	public static final String CODE_A2 = "A2";
	public static final String CODE_A3 = "A3";
	public static final String CODE_A4 = "A4";
	
	public static String getCategoryCodeFromCategoryName(String categoryName) {
		if(NAME_G1.equals(categoryName))
			return CODE_G1;
		
		if(NAME_O2.equals(categoryName))
			return CODE_O2;
		if(NAME_O3.equals(categoryName))
			return CODE_O3;
		
		if(NAME_C2.equals(categoryName))
			return CODE_C2;
		if(NAME_C3.equals(categoryName))
			return CODE_C3;
		
		if(NAME_T1.equals(categoryName))
			return CODE_T1;
		if(NAME_T2.equals(categoryName))
			return CODE_T2;
		if(NAME_T3.equals(categoryName))
			return CODE_T3;
		if(NAME_T4.equals(categoryName))
			return CODE_T4;
		if(NAME_T5.equals(categoryName))
			return CODE_T5;
		if(NAME_T6.equals(categoryName))
			return CODE_T6;
		if(NAME_T8.equals(categoryName))
			return CODE_T8;
		if(NAME_T9.equals(categoryName))
			return CODE_T9;
		return null;
	}
	
	public static String getAssistantCategoryCodeFromHolderCategoryCode(String holderCategoryCode) {
		if(StringHelper.isBlank(holderCategoryCode))
			return null;
		if(holderCategoryCode.startsWith("G"))
			return CODE_A1;
		if(holderCategoryCode.startsWith("O"))
			return CODE_A2;
		if(holderCategoryCode.startsWith("C"))
			return CODE_A3;
		if(holderCategoryCode.startsWith("T"))
			return CODE_A4;
		return null;
	}
}