package ci.gouv.dgbf.system.actor.server.persistence.entities;

import java.io.Serializable;
import java.util.Collection;
import java.util.stream.Collectors;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;
import javax.validation.constraints.NotNull;

import org.cyk.utility.__kernel__.collection.CollectionHelper;
import org.cyk.utility.__kernel__.object.__static__.persistence.AbstractIdentifiableSystemScalarStringImpl;
import org.cyk.utility.__kernel__.string.StringHelper;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;

@Getter @Setter @Accessors(chain=true) @NoArgsConstructor
@Entity @Table(name=ScopeFunctionExecutionImputation.TABLE_NAME,uniqueConstraints = {
		@UniqueConstraint(name=ScopeFunctionExecutionImputation.TABLE_NAME+"_"+ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION+"_"
				+ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION+"_UK"
				,columnNames = {ScopeFunctionExecutionImputation.COLUMN_SCOPE_FUNCTION,ScopeFunctionExecutionImputation.COLUMN_EXECUTION_IMPUTATION})
})
public class ScopeFunctionExecutionImputation extends AbstractIdentifiableSystemScalarStringImpl implements Serializable {
	private static final long serialVersionUID = 1L;
	
	@ManyToOne @JoinColumn(name = COLUMN_SCOPE_FUNCTION) @NotNull private ScopeFunction scopeFunction;
	@ManyToOne @JoinColumn(name = COLUMN_EXECUTION_IMPUTATION) @NotNull private ExecutionImputation executionImputation;
	
	@Transient private String scopeFunctionIdentifier;
	@Transient private String executionImputationIdentifier;
	
	@Override
	public ScopeFunctionExecutionImputation setIdentifier(String identifier) {
		return (ScopeFunctionExecutionImputation) super.setIdentifier(identifier);
	}
	
	public static Collection<ScopeFunctionExecutionImputation> filterBy(ExecutionImputation executionImputation,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		if(executionImputation == null || CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
			return null;
		return scopeFunctionExecutionImputations.stream().filter(x -> x.getExecutionImputation().equals(executionImputation)).collect(Collectors.toList());
	}
	
	public static ScopeFunctionExecutionImputation find(ScopeFunction scopeFunction,ExecutionImputation executionImputation,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		if(scopeFunction == null || executionImputation == null || CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
			return null;
		Collection<ScopeFunctionExecutionImputation> collection = scopeFunctionExecutionImputations.stream()
				.filter(x -> x.getScopeFunction().equals(scopeFunction) && x.getExecutionImputation().equals(executionImputation)).collect(Collectors.toList());
		if(CollectionHelper.getSize(collection) > 1)
			throw new RuntimeException("Too much result found");
		return CollectionHelper.getFirst(collection);
	}
	
	public static ScopeFunctionExecutionImputation find(String functionCode,Collection<ScopeFunctionExecutionImputation> scopeFunctionExecutionImputations) {
		if(StringHelper.isBlank(functionCode) || CollectionHelper.isEmpty(scopeFunctionExecutionImputations))
			return null;
		Collection<ScopeFunctionExecutionImputation> collection = scopeFunctionExecutionImputations.stream()
				.filter(x -> x.getScopeFunction().getFunction().getCode().equals(functionCode)).collect(Collectors.toList());
		if(CollectionHelper.getSize(collection) > 1)
			throw new RuntimeException("Too much result found");
		return CollectionHelper.getFirst(collection);
	}
	
	@Override
	public String toString() {
		return scopeFunction+"/"+executionImputation;
	}
	
	public static final String FIELD_SCOPE_FUNCTION = "scopeFunction";
	public static final String FIELD_EXECUTION_IMPUTATION = "executionImputation";
	public static final String FIELD_SCOPE_FUNCTION_IDENTIFIER = "scopeFunctionIdentifier";
	public static final String FIELD_EXECUTION_IMPUTATION_IDENTIFIER = "executionImputationIdentifier";
		
	public static final String TABLE_NAME = "POSTE_IMPUTATION";
	
	public static final String COLUMN_IDENTIFIER = "IDENTIFIANT";
	public static final String COLUMN_SCOPE_FUNCTION = "POSTE";
	public static final String COLUMN_EXECUTION_IMPUTATION = "IMPUTATION";
}